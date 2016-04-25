/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.io

import java.io.{ File, IOException }

import breeze.linalg.{ DenseMatrix, DenseVector }
import niftijio.{ NiftiHeader, NiftiVolume }
import scalismo.common.{ RealSpace, Scalar }
import scalismo.geometry._
import scalismo.image.{ DiscreteImageDomain, DiscreteScalarImage }
import scalismo.registration._
import scalismo.utils.{ CanConvertToVtk, ImageConversion, VtkHelpers }
import spire.math.{ UByte, UInt, UShort }
import vtk._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

/**
 * Implements methods for reading and writing D-dimensional images
 *
 * '''WARNING! WE ARE USING an LPS WORLD COORDINATE SYSTEM'''
 *
 * VTK file format does not indicate the orientation of the image.
 * Therefore, when reading from VTK, we assume that it is in RAI orientation.
 * Hence, no magic is done, the same information (coordinates) present in the
 * VTK file header are directly mapped to our coordinate system.
 *
 * This is also the case when writing VTK. Our image domain information (origin, spacing ..) is mapped
 * directly into the written VTK file header.
 *
 * This is however not the case for Nifti files! Nifti file headers contain an affine transform from the ijk
 * image coordinates to an RAS World Coordinate System (therefore supporting different image orientations).
 * In order to read Nifti files coherently, we need to adapt the obtained RAS coordinates to our LPS system :
 *
 * This is done by mirroring the first two dimensions of each point after applying the affine transform
 *
 * The same mirroring is done again when writing an image to the Nifti format.
 *
 *
 * '''Important for oblique images :'''
 * The Nifti standard supports oblique images, that is images with a bounding box rotated compared to the world dimensions.
 * Scalismo does not support such images. For such images, we offer the user a possibility to resample the image to
 * a domain aligned with the world dimensions and with an RAI orientation. The integrity of the oblique image will be contained
 * in the resampled one. This functionality can be activated by setting a flag appropriately in the [[scalismo.io.ImageIO.read3DScalarImage]] method.
 *
 *
 * '''Note on Nifti's qform and sform :'''
 *
 * As mentioned above, the Nifti header contains a transform from the unit ijk grid to the RAS world coordinates of the grid.
 * This transform can be encoded in 2 entries of the Nifti header, the qform and the sform. In some files, these 2 entries can both be present,
 * and in some cases could even indicate different transforms. In Scalismo, when such a case happens, we favour the sform entry by default.
 * If you wish instead to favour the qform transform, you can do so by setting a flag appropriately in the [[scalismo.io.ImageIO.read3DScalarImage]] method.
 *
 *
 * ''' Documentation on orientation :'''
 *
 * http://www.grahamwideman.com/gw/brain/orientation/orientterms.htm
 *
 * http://www.slicer.org/slicerWiki/index.php/Coordinate_systems
 *
 * http://brainder.org/2012/09/23/the-nifti-file-format/
 *
 */

object ImageIO {

  /**
   * An enumeration comprising all the data types that we can read and write, in VTK and Nifti formats.
   */
  object ScalarType extends Enumeration {

    import NiftiHeader._
    import VtkHelpers._

    import scala.language.implicitConversions

    protected case class Val[O: Scalar: ClassTag](vtkId: Int, niftiId: Short) extends super.Val

    implicit def valueToVal[T](x: Value): Val[T] = x.asInstanceOf[Val[T]]

    val Byte = Val[Byte](VTK_CHAR, NIFTI_TYPE_INT8)
    val Short = Val[Short](VTK_SHORT, NIFTI_TYPE_INT16)
    val Int = Val[Int](VTK_INT, NIFTI_TYPE_INT32)
    val Float = Val[Float](VTK_FLOAT, NIFTI_TYPE_FLOAT32)
    val Double = Val[Double](VTK_DOUBLE, NIFTI_TYPE_FLOAT64)
    val UByte = Val[UByte](VTK_UNSIGNED_CHAR, NIFTI_TYPE_UINT8)
    val UShort = Val[UShort](VTK_UNSIGNED_SHORT, NIFTI_TYPE_UINT16)
    val UInt = Val[UInt](VTK_UNSIGNED_INT, NIFTI_TYPE_UINT32)

    /**
     * Return the ScalarType value corresponding to a given type
     * @tparam T a scalar type
     * @return the corresponding ScalarType value
     * @throws IllegalArgumentException if no corresponding value was found.
     */
    def fromType[T: Scalar: TypeTag]: Value = {
      typeOf[T] match {
        case t if t =:= typeOf[Byte] => Byte
        case t if t =:= typeOf[Short] => Short
        case t if t =:= typeOf[Int] => Int
        case t if t =:= typeOf[Float] => Float
        case t if t =:= typeOf[Double] => Double
        case t if t =:= typeOf[UByte] => UByte
        case t if t =:= typeOf[UShort] => UShort
        case t if t =:= typeOf[UInt] => UInt
        case _ => throw new IllegalArgumentException(s"Unsupported datatype ${typeOf[T]}")
      }
    }

    /**
     * Return the ScalarType value corresponding to a given VTK type constant
     * @param vtkId a VTK type constant
     * @return the corresponding ScalarType value
     * @throws IllegalArgumentException if no corresponding value was found.
     */
    def fromVtkId(vtkId: Int): Value = {
      // there are two ways in VTK to represent a (signed) byte.
      if (vtkId == VTK_SIGNED_CHAR) Byte
      else values.find(v => v.vtkId == vtkId).getOrElse(throw new IllegalArgumentException(s"Unsupported VTK ID $vtkId"))
    }

    /**
     * Return the ScalarType value corresponding to a given Nifti type constant
     * @param niftiId a Nifti type constant
     * @return the corresponding ScalarType value
     * @throws IllegalArgumentException if no corresponding value was found.
     */
    def fromNiftiId(niftiId: Short): ScalarType.Value = {
      values.find(v => v.niftiId == niftiId).getOrElse(throw new IllegalArgumentException(s"Unsupported Nifti ID $niftiId"))
    }

    /**
     * Return the ScalarType value corresponding to the data present in a given file. Only .vtk, .nii and .nia files are supported.
     * @param file the file to check
     * @return the scalar type present in the given file, wrapped in a [[scala.util.Success]], or a [[scala.util.Failure]] explaining the error.
     */
    def ofFile(file: File): Try[ScalarType.Value] = {
      val fn = file.getName
      if (fn.endsWith(".nii") || fn.endsWith(".nia")) {
        FastReadOnlyNiftiVolume.getScalarType(file).map(ScalarType.fromNiftiId)
      } else if (fn.endsWith(".vtk")) Try {
        val reader = new vtkStructuredPointsReader
        reader.SetFileName(file.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          reader.Delete()
          throw new IOException(s"Failed to read vtk file ${file.getAbsolutePath}. (error code from vtkReader = $errCode)")
        }
        val st = reader.GetOutput().GetScalarType()
        reader.Delete()
        // prevent memory leaks
        vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
        ScalarType.fromVtkId(st)
      }
      else {
        Failure(new Exception(s"File $file: unsupported file extension"))
      }
    }
  }

  trait WriteNifti[D <: Dim] {
    def write[A: Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[D, A], f: File): Try[Unit]
  }

  implicit object DiscreteScalarImage3DNifti extends WriteNifti[_3D] {
    def write[A: Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[_3D, A], f: File): Try[Unit] = {
      writeNifti[A](img, f)
    }
  }

  /**
   * Read a 3D Scalar Image
   * @param file  image file to be read
   * @param resampleOblique  flag to resample oblique images. This is only required when reading Nifti files containing an oblique image. See documentation above [[ImageIO]].
   * @param favourQform  flag to favour the qform Nifti header entry over the sform one (which is by default favoured). See documentation above [[ImageIO]].
   * @tparam S Voxel type of the image
   *
   */
  def read3DScalarImage[S: Scalar: TypeTag: ClassTag](file: File, resampleOblique: Boolean = false, favourQform: Boolean = false): Try[DiscreteScalarImage[_3D, S]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException(s"Failed to read vtk file ${f.getAbsolutePath}. " +
            s"(error code from vtkReader = $errCode)"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsToScalarImage[_3D, S](sp)
        reader.Delete()
        sp.Delete()
        // unfortunately, there may still be VTK leftovers, so run garbage collection
        vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
        img
      case f if f.getAbsolutePath.endsWith(".nii") || f.getAbsolutePath.endsWith(".nia") =>
        readNifti[S](f, resampleOblique, favourQform)
      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  /**
   * Read a 3D Scalar Image, and possibly convert it to the requested voxel type.
   *
   * This method is similar to the [[read3DScalarImage]] method, except that it will convert the image to the requested voxel type if
   * the type in the file is different, whereas [[read3DScalarImage]] will throw an exception in that case.
   *
   * @param file  image file to be read
   * @param resampleOblique  flag to resample oblique images. This is only required when reading Nifti files containing an oblique image. See documentation above [[ImageIO]].
   * @param favourQform  flag to favour the qform Nifti header entry over the sform one (which is by default favoured). See documentation above [[ImageIO]].
   * @tparam S Voxel type of the image
   *
   */
  def read3DScalarImageAsType[S: Scalar: TypeTag: ClassTag](file: File, resampleOblique: Boolean = false, favourQform: Boolean = false): Try[DiscreteScalarImage[_3D, S]] = {
    def loadAs[T: Scalar: TypeTag: ClassTag]: Try[DiscreteScalarImage[_3D, T]] = {
      read3DScalarImage[T](file, resampleOblique, favourQform)
    }

    val result = (for {
      fileScalarType <- ScalarType.ofFile(file)
    } yield {
      val expectedScalarType = ScalarType.fromType[S]
      if (expectedScalarType == fileScalarType) {
        loadAs[S]
      } else {
        val s = implicitly[Scalar[S]]
        fileScalarType match {
          case ScalarType.Byte => loadAs[Byte].map(_.map(s.fromByte))
          case ScalarType.Short => loadAs[Short].map(_.map(s.fromShort))
          case ScalarType.Int => loadAs[Int].map(_.map(s.fromInt))
          case ScalarType.Float => loadAs[Float].map(_.map(s.fromFloat))
          case ScalarType.Double => loadAs[Double].map(_.map(s.fromDouble))
          case ScalarType.UByte => loadAs[UByte].map(_.map(u => s.fromShort(u.toShort)))
          case ScalarType.UShort => loadAs[UShort].map(_.map(u => s.fromInt(u.toInt)))
          case ScalarType.UInt => loadAs[UInt].map(_.map(u => s.fromLong(u.toLong)))

          case _ => Failure(new IllegalArgumentException(s"unknown scalar type $fileScalarType"))
        }
      }
    }).flatten
    result
  }

  /**
   * Read a 2D Scalar Image
   * @param file  image file to be read
   * @tparam S Voxel type of the image
   *
   */
  def read2DScalarImage[S: Scalar: ClassTag: TypeTag](file: File): Try[DiscreteScalarImage[_2D, S]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException(s"Failed to read vtk file ${file.getAbsolutePath}. " +
            s"(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsToScalarImage[_2D, S](sp)
        reader.Delete()
        sp.Delete()
        // unfortunately, there may still be VTK leftovers, so run garbage collection
        vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
        img

      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  /**
   * Read a 2D Scalar Image, and possibly convert it to the requested voxel type.
   *
   * This method is similar to the [[read2DScalarImage]] method, except that it will convert the image to the requested voxel type if
   * the type in the file is different, whereas [[read2DScalarImage]] will throw an exception in that case.
   *
   * @param file  image file to be read
   * @tparam S Voxel type of the image
   *
   */
  def read2DScalarImageAsType[S: Scalar: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage[_2D, S]] = {
    def loadAs[T: Scalar: TypeTag: ClassTag]: Try[DiscreteScalarImage[_2D, T]] = {
      read2DScalarImage[T](file)
    }

    val result = (for {
      fileScalarType <- ScalarType.ofFile(file)
    } yield {
      val expectedScalarType = ScalarType.fromType[S]
      if (expectedScalarType == fileScalarType) {
        loadAs[S]
      } else {
        val s = implicitly[Scalar[S]]
        fileScalarType match {
          case ScalarType.Byte => loadAs[Byte].map(_.map(s.fromByte))
          case ScalarType.Short => loadAs[Short].map(_.map(s.fromShort))
          case ScalarType.Int => loadAs[Int].map(_.map(s.fromInt))
          case ScalarType.Float => loadAs[Float].map(_.map(s.fromFloat))
          case ScalarType.Double => loadAs[Double].map(_.map(s.fromDouble))
          case ScalarType.UByte => loadAs[UByte].map(_.map(u => s.fromShort(u.toShort)))
          case ScalarType.UShort => loadAs[UShort].map(_.map(u => s.fromInt(u.toInt)))
          case ScalarType.UInt => loadAs[UInt].map(_.map(u => s.fromLong(u.toLong)))

          case _ => Failure(new IllegalArgumentException(s"unknown scalar type $fileScalarType"))
        }
      }
    }).flatten
    result
  }

  private def readNifti[S: Scalar: TypeTag: ClassTag](file: File, resampleOblique: Boolean, favourQform: Boolean): Try[DiscreteScalarImage[_3D, S]] = {

    for {

      volume <- FastReadOnlyNiftiVolume.read(file.getAbsolutePath)
      pair <- computeNiftiWorldToVoxelTransforms(volume, favourQform)
    } yield {
      val expectedScalarType = ScalarType.fromType[S]
      val foundScalarType = ScalarType.fromNiftiId(volume.header.datatype)
      if (expectedScalarType != foundScalarType) {
        throw new IllegalArgumentException(s"Invalid scalar type (expected $expectedScalarType, found $foundScalarType)")
      }

      val (transVoxelToWorld, _) = pair

      val nx = volume.header.dim(1)
      val ny = volume.header.dim(2)
      val nz = volume.header.dim(3)
      var dim = volume.header.dim(4)

      if (dim == 0)
        dim = 1

      /* figure out the anisotropic scaling factor */
      val s = volume.header.pixdim

      // figure out if the ijk to xyz_RAS transform does mirror: determinant of the linear transform

      val augmentedMatrix = transformMatrixFromNifti(volume, favourQform).get // get is safe in here
      val linearTransMatrix = augmentedMatrix(0 to 2, 0 to 2)

      val mirrorScale = breeze.linalg.det(linearTransMatrix).signum.toDouble

      val spacing = DenseVector(s(1), s(2), s(3) * mirrorScale)

      val anisotropicScaling = new AnisotropicScalingSpace[_3D].transformForParameters(spacing)

      /* get a rigid registration by mapping a few points */
      val origPs = List(Point(0, 0, nz), Point(0, ny, 0), Point(0, ny, nz), Point(nx, 0, 0), Point(nx, 0, nz), Point(nx, ny, 0), Point(nx, ny, nz))
      val scaledPS = origPs.map(anisotropicScaling)
      val imgPs = origPs.map(transVoxelToWorld)

      val rigidReg = LandmarkRegistration.rigid3DLandmarkRegistration((scaledPS zip imgPs).toIndexedSeq)
      val transform = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(DenseVector(rigidReg.parameters.data ++ spacing.data))

      val rotationResiduals = rigidReg.parameters(3 to 5).toArray.map { a =>
        val rest = math.abs(a) % (math.Pi * 0.5)
        math.min(rest, (math.Pi * 0.5) - rest)
      }

      // if the image is oblique and the resampling flag unset, throw an exception
      if (!resampleOblique && rotationResiduals.exists(_ >= 0.001)) {
        throw new Exception("The image orientation seems to be oblique, which is not supported by default in scalismo. To read the image anyway, activate the resampleOblique flag. This will resample the image to an RAI oriented one.")
      }

      /* Test that were able to reconstruct the transform */
      val approxErrors = (origPs.map(transform) zip imgPs).map { case (o, i) => (o - i).norm }
      if (approxErrors.max > 0.01f) throw new Exception("Unable to approximate Nifti affine transform with anisotropic similarity transform")
      else {
        val newDomain = DiscreteImageDomain[_3D](IntVector(nx, ny, nz), transform)
        val im = DiscreteScalarImage(newDomain, volume.dataAsScalarArray)

        // if the domain is rotated, we resample the image to RAI voxel ordering
        if (rotationResiduals.exists(_ >= 0.001)) {
          // using our vtk conversion, we get  resampled structured point data that fully contains the original image and is RAI ordered
          val sp = ImageConversion.imageToVtkStructuredPoints[_3D, S](im)
          ImageConversion.vtkStructuredPointsToScalarImage[_3D, S](sp).get
        } else im
      }
    }
  }

  /* Returns the augmented matrix of the affine transform from ijk to xyz_RAS
   * The logic is based on: http://brainder.org/2012/09/23/the-nifti-file-format/
   * (section "Orientation information").
   */
  private[this] def transformMatrixFromNifti(volume: FastReadOnlyNiftiVolume, favourQform: Boolean): Try[DenseMatrix[Double]] = {
    (volume.header.qform_code, volume.header.sform_code) match {
      case (0, 0) => // Method 1
        val data = Array.fill(16)(0.0d)
        // using homogeneous coordinates: set the last matrix element to 1
        data(15) = 1
        // diagonal matrix, with the diagonal values initialized to pixdim[i+1]
        for (i <- 0 until 3) {
          // shortcut for n*i + i, since we know that n==4
          data(i * 5) = volume.header.pixdim(i + 1)
        }
        Success(DenseMatrix.create(4, 4, data))
      case (q, 0) if q != 0 => // Method 2
        Success(DenseMatrix.create(4, 4, volume.header.qform_to_mat44.flatten).t)
      case (q, s) if s != 0 => // Method 3
        //Attention: we're by default ignoring the q value here, and solely basing the decision on s != 0, unless the user says so
        if (favourQform)
          Success(DenseMatrix.create(4, 4, volume.header.qform_to_mat44.flatten).t)
        else
          Success(DenseMatrix.create(4, 4, volume.header.sformArray).t)
    }
  }

  /**
   * returns transformations from voxel to World coordinates and its inverse
   */

  private[this] def computeNiftiWorldToVoxelTransforms(volume: FastReadOnlyNiftiVolume, favourQform: Boolean): Try[(Transformation[_3D], Transformation[_3D])] = {
    var dim = volume.header.dim(4)

    if (dim == 0)
      dim = 1

    // check this page http://brainder.org/2012/09/23/the-nifti-file-format/
    // for details about the nifti format

    transformMatrixFromNifti(volume, favourQform).map { affineTransMatrix =>

      val t = new Transformation[_3D] {
        override val domain = RealSpace[_3D]
        override val f = (x: Point[_3D]) => {
          val xh = DenseVector(x(0), x(1), x(2), 1.0)
          val t: DenseVector[Double] = affineTransMatrix * xh

          // We flip after applying the transform as Nifti uses RAS coordinates
          Point(t(0).toFloat * -1f, t(1).toFloat * -1f, t(2).toFloat)
        }
      }

      val affineTransMatrixInv: DenseMatrix[Double] = breeze.linalg.inv(affineTransMatrix)
      val tinv = new Transformation[_3D] {
        override val f = (x: Point[_3D]) => {
          // Here as it is the inverse, we flip before applying the affine matrix
          val xh: DenseVector[Double] = DenseVector(x(0) * -1.0, x(1) * -1, x(2), 1.0)
          val t: DenseVector[Float] = (affineTransMatrixInv * xh).map(_.toFloat)
          Point(t(0), t(1), t(2))
        }
        override val domain = RealSpace[_3D]
      }

      (t, tinv)
    }
  }

  def writeNifti[S: Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[_3D, S], file: File): Try[Unit] = {

    val scalarConv = implicitly[Scalar[S]]

    val domain = img.domain
    val size = domain.size
    val dim = 1

    Try {

      val volume = new NiftiVolume(size(0), size(1), size(2), dim)

      // the data

      for (d <- 0 until dim; k <- 0 until size(2); j <- 0 until size(1); i <- 0 until size(0)) {
        volume.data.set(i, j, k, d, scalarConv.toDouble(img(IntVector(i, j, k))))
      }

      val innerAffineMatrix = DiscreteImageDomain.computeInnerAffineMatrix(img.domain)
      val M = DenseMatrix.zeros[Double](4, 4)

      M(0, 0) = innerAffineMatrix(0, 0) * -1f
      M(0, 1) = innerAffineMatrix(0, 1) * -1f
      M(0, 2) = innerAffineMatrix(0, 2) * -1f
      M(0, 3) = -domain.origin(0)
      M(1, 0) = innerAffineMatrix(1, 0) * -1f
      M(1, 1) = innerAffineMatrix(1, 1) * -1f
      M(1, 2) = innerAffineMatrix(1, 2) * -1f
      M(1, 3) = -domain.origin(1)
      M(2, 0) = innerAffineMatrix(2, 0)
      M(2, 1) = innerAffineMatrix(2, 1)
      M(2, 2) = innerAffineMatrix(2, 2)
      M(2, 3) = domain.origin(2)
      M(3, 3) = 1

      // the header
      volume.header.setDatatype(ScalarType.fromType[S].niftiId)
      volume.header.qform_code = 0
      volume.header.sform_code = 2 // TODO check me that this is right

      val data = M.t.toDenseVector.toArray.map(_.toFloat)

      volume.header.srow_x = data.take(4)
      volume.header.srow_y = data.slice(4, 8)
      volume.header.srow_z = data.slice(8, 12)
      volume.header.pixdim(1) = domain.spacing(0).toFloat
      volume.header.pixdim(2) = domain.spacing(1).toFloat
      volume.header.pixdim(3) = domain.spacing(2).toFloat

      volume.write(file.getAbsolutePath)
    }
  }

  def writeVTK[D <: Dim: NDSpace: CanConvertToVtk, S: Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[D, S], file: File): Try[Unit] = {

    val imgVtk = ImageConversion.imageToVtkStructuredPoints(img)

    val writer = new vtkStructuredPointsWriter()
    writer.SetInputData(imgVtk)
    writer.SetFileName(file.getAbsolutePath)
    writer.SetFileTypeToBinary()
    writer.Update()
    val errorCode = writer.GetErrorCode()

    // unfortunately, there will probably still be VTK leftovers from objects allocated
    // outside of our control, so run garbage collection
    vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)

    if (errorCode != 0) {
      Failure(new IOException(s"Error writing vtk file ${file.getAbsolutePath} (error code $errorCode"))
    } else {
      Success(())
    }
  }

}
