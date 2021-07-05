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

import java.io.{File, IOException}

import breeze.linalg.{diag, DenseMatrix, DenseVector}
import niftijio.{NiftiHeader, NiftiVolume}
import scalismo.common.{RealSpace, Scalar}
import scalismo.geometry._
import scalismo.image.{DiscreteImage, DiscreteImageDomain, StructuredPoints, StructuredPoints3D}
import scalismo.registration._
import scalismo.transformations.{RotationSpace3D, Transformation}
import scalismo.utils.{CanConvertToVtk, ImageConversion, VtkHelpers}
import spire.math.{UByte, UInt, UShort}
import vtk._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

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

  trait WriteNifti[D] {
    def write[A: Scalar: ClassTag](img: DiscreteImage[D, A], f: File): Try[Unit]
  }

  implicit object DiscreteScalarImage3DNifti extends WriteNifti[_3D] {
    def write[A: Scalar: ClassTag](img: DiscreteImage[_3D, A], f: File): Try[Unit] = {
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
  def read3DScalarImage[S: Scalar: ClassTag](
    file: File,
    resampleOblique: Boolean = false,
    favourQform: Boolean = false
  ): Try[DiscreteImage[_3D, S]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(
            new IOException(
              s"Failed to read vtk file ${f.getAbsolutePath}. " +
                s"(error code from vtkReader = $errCode)"
            )
          )
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
  def read3DScalarImageAsType[S: Scalar: ClassTag](
    file: File,
    resampleOblique: Boolean = false,
    favourQform: Boolean = false
  ): Try[DiscreteImage[_3D, S]] = {
    def loadAs[T: Scalar: ClassTag]: Try[DiscreteImage[_3D, T]] = {
      read3DScalarImage[T](file, resampleOblique, favourQform)
    }

    val result = (for {
      fileScalarType <- ScalarDataType.ofFile(file)
    } yield {
      val expectedScalarType = ScalarDataType.fromType[S]
      if (expectedScalarType == fileScalarType) {
        loadAs[S]
      } else {
        val s = implicitly[Scalar[S]]
        fileScalarType match {
          case ScalarDataType.Byte   => loadAs[Byte].map(_.map(s.fromByte))
          case ScalarDataType.Short  => loadAs[Short].map(_.map(s.fromShort))
          case ScalarDataType.Int    => loadAs[Int].map(_.map(s.fromInt))
          case ScalarDataType.Float  => loadAs[Float].map(_.map(s.fromFloat))
          case ScalarDataType.Double => loadAs[Double].map(_.map(s.fromDouble))
          case ScalarDataType.UByte  => loadAs[UByte].map(_.map(u => s.fromShort(u.toShort)))
          case ScalarDataType.UShort => loadAs[UShort].map(_.map(u => s.fromInt(u.toInt)))
          case ScalarDataType.UInt   => loadAs[UInt].map(_.map(u => s.fromLong(u.toLong)))

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
  def read2DScalarImage[S: Scalar: ClassTag](file: File): Try[DiscreteImage[_2D, S]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(
            new IOException(
              s"Failed to read vtk file ${file.getAbsolutePath}. " +
                s"(error code from vtkReader = $errCode"
            )
          )
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
  def read2DScalarImageAsType[S: Scalar: ClassTag](file: File): Try[DiscreteImage[_2D, S]] = {
    def loadAs[T: Scalar: ClassTag]: Try[DiscreteImage[_2D, T]] = {
      read2DScalarImage[T](file)
    }

    val result = (for {
      fileScalarType <- ScalarDataType.ofFile(file)
    } yield {
      val expectedScalarType = ScalarDataType.fromType[S]
      if (expectedScalarType == fileScalarType) {
        loadAs[S]
      } else {
        val s = implicitly[Scalar[S]]
        fileScalarType match {
          case ScalarDataType.Byte   => loadAs[Byte].map(_.map(s.fromByte))
          case ScalarDataType.Short  => loadAs[Short].map(_.map(s.fromShort))
          case ScalarDataType.Int    => loadAs[Int].map(_.map(s.fromInt))
          case ScalarDataType.Float  => loadAs[Float].map(_.map(s.fromFloat))
          case ScalarDataType.Double => loadAs[Double].map(_.map(s.fromDouble))
          case ScalarDataType.UByte  => loadAs[UByte].map(_.map(u => s.fromShort(u.toShort)))
          case ScalarDataType.UShort => loadAs[UShort].map(_.map(u => s.fromInt(u.toInt)))
          case ScalarDataType.UInt   => loadAs[UInt].map(_.map(u => s.fromLong(u.toLong)))

          case _ => Failure(new IllegalArgumentException(s"unknown scalar type $fileScalarType"))
        }
      }
    }).flatten
    result
  }

  private def readNifti[S: Scalar: ClassTag](file: File,
                                             resampleOblique: Boolean,
                                             favourQform: Boolean): Try[DiscreteImage[_3D, S]] = {

    for {
      volume <- FastReadOnlyNiftiVolume.read(file.getAbsolutePath)
      transVoxelToWorld <- computeNiftiWorldToVoxelTransforms(volume, favourQform)
      image <- createImageWithRightOrientation(resampleOblique, favourQform, volume, transVoxelToWorld)
    } yield {
      image
    }
  }

  private def createImageWithRightOrientation[S: Scalar: ClassTag](
    resampleOblique: Boolean,
    favourQform: Boolean,
    volume: FastReadOnlyNiftiVolume,
    transVoxelToWorld: Transformation[_3D]
  ): Try[DiscreteImage[_3D, S]] = {

    val expectedScalarType = ScalarDataType.fromType[S]
    // If a volume has a transform, we always treat it as a float image and convert accordingly.
    // Otherwise we take the type that is found in the nifty header
    val foundScalarType =
      if (volume.hasTransform) ScalarDataType.fromType[Float]
      else ScalarDataType.fromNiftiId(volume.header.datatype)

    if (expectedScalarType != foundScalarType) {
      Failure(new IllegalArgumentException(
        s"Invalid scalar type (expected $expectedScalarType, found $foundScalarType)"
      ))
    } else {

      // First we compute origin, spacing and size
      val origin = transVoxelToWorld(Point3D(0, 0, 0))

      val nx = volume.header.dim(1)
      val ny = volume.header.dim(2)
      val nz = volume.header.dim(3)
      val dim = if (volume.header.dim(4) == 0) 1 else volume.header.dim(4)

      val spacing = EuclideanVector3D(volume.header.pixdim(1), volume.header.pixdim(2), volume.header.pixdim(3))
      val size = IntVector(nx, ny, nz)

      // Then we need to figure out the image orientation. To fix the orientation, we compute
      // three unit vectors i, j, k, which determine the coordinate system
      val augmentedMatrix = transformMatrixFromNifti(volume, favourQform).get // get is safe in here
      val linearTransMatrix = augmentedMatrix(0 to 2, 0 to 2)

      def toUnitVec(v: EuclideanVector[_3D]): EuclideanVector[_3D] = {
        v * (1.0 / v.norm)
      }

      // The i and j vector point in the opposite direction from what is given in the nifti system, as
      // nifti uses RAS and we use the LPS coordinate system
      val iVec = toUnitVec(EuclideanVector.fromBreezeVector[_3D](linearTransMatrix * DenseVector(-1.0, 0.0, 0.0)))
      val jVec = toUnitVec(EuclideanVector.fromBreezeVector[_3D](linearTransMatrix * DenseVector(0.0, -1.0, 0.0)))
      val kVec = toUnitVec(EuclideanVector.fromBreezeVector[_3D](linearTransMatrix * DenseVector(0.0, 0.0, 1.0)))

      // now we assemble everything
      val newDomain = DiscreteImageDomain(
        StructuredPoints3D(origin, EuclideanVector(spacing(0), spacing(1), spacing(2)), size, iVec, jVec, kVec)
      )
      val im = DiscreteImage(newDomain, volume.dataAsScalarArray)

      // Finally, there is a special case.  if the domain is rotated, we resample the image to RAI voxel ordering
      val isOblique = Math.abs(Math.abs(iVec.dot(EuclideanVector3D(1, 0, 0))) - 1.0) > 1e-5
      if (isOblique && !resampleOblique) {
        Failure(
          new Exception(
            "The image orientation seems to be oblique, which is not supported by default in scalismo. " +
              "To read the image anyway, activate the resampleOblique flag. " +
              "This will resample the image to an RAI oriented one."
          )
        )
      } else if (isOblique && resampleOblique) {
        // using our vtk conversion, we get  resampled structured point data that fully contains the original image
        // and is RAI ordered
        val sp = ImageConversion.imageToVtkStructuredPoints[_3D, S](im)
        ImageConversion.vtkStructuredPointsToScalarImage[_3D, S](sp)
      } else {
        Success(im)
      }
    }
  }

  /* Returns the augmented matrix of the affine transform from ijk to xyz_RAS
   * The logic is based on: http://brainder.org/2012/09/23/the-nifti-file-format/
   * (section "Orientation information").
   */
  private[this] def transformMatrixFromNifti(volume: FastReadOnlyNiftiVolume,
                                             favourQform: Boolean): Try[DenseMatrix[Double]] = {
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
   * returns transformation from voxel to World coordinates and its inverse
   */
  private[this] def computeNiftiWorldToVoxelTransforms(
    volume: FastReadOnlyNiftiVolume,
    favourQform: Boolean
  ): Try[Transformation[_3D]] = {
    var dim = volume.header.dim(4)

    if (dim == 0)
      dim = 1

    // check this page http://brainder.org/2012/09/23/the-nifti-file-format/
    // for details about the nifti format

    transformMatrixFromNifti(volume, favourQform).map { affineTransMatrix =>
      {
        val domain = RealSpace[_3D]
        val f = (x: Point[_3D]) => {
          val xh = DenseVector(x(0), x(1), x(2), 1.0)
          val t: DenseVector[Double] = affineTransMatrix * xh

          // We flip after applying the transform as Nifti uses RAS coordinates
          Point(t(0).toFloat * -1f, t(1).toFloat * -1f, t(2).toFloat)
        }
        Transformation(domain, f)
      }
    }
  }

  def writeNifti[S: Scalar: ClassTag](img: DiscreteImage[_3D, S], file: File): Try[Unit] = {

    val scalarConv = implicitly[Scalar[S]]

    val domain = img.domain
    val size = domain.pointSet.size
    val dim = 1

    Try {

      val volume = new NiftiVolume(size(0), size(1), size(2), dim)

      // the data

      for (d <- 0 until dim; k <- 0 until size(2); j <- 0 until size(1); i <- 0 until size(0)) {
        volume.data.set(i, j, k, d, scalarConv.toDouble(img(IntVector(i, j, k))))
      }

      def computeInnerAffineMatrix(domain: StructuredPoints[_3D]): DenseMatrix[Double] = {
        val scalingParams = DenseVector[Double](domain.spacing(0), domain.spacing(1), domain.spacing(2))
        val scalingMatrix = diag(scalingParams)
        val innerAffineMatrix = RotationSpace3D
          .eulerAnglesToRotMatrix(0, 0, 0) // TODO fix me
          .toBreezeMatrix * scalingMatrix
        innerAffineMatrix
      }

      val innerAffineMatrix = computeInnerAffineMatrix(img.domain.pointSet)
      val M = DenseMatrix.zeros[Double](4, 4)

      M(0, 0) = innerAffineMatrix(0, 0) * -1f
      M(0, 1) = innerAffineMatrix(0, 1) * -1f
      M(0, 2) = innerAffineMatrix(0, 2) * -1f
      M(0, 3) = -domain.pointSet.origin(0)
      M(1, 0) = innerAffineMatrix(1, 0) * -1f
      M(1, 1) = innerAffineMatrix(1, 1) * -1f
      M(1, 2) = innerAffineMatrix(1, 2) * -1f
      M(1, 3) = -domain.pointSet.origin(1)
      M(2, 0) = innerAffineMatrix(2, 0)
      M(2, 1) = innerAffineMatrix(2, 1)
      M(2, 2) = innerAffineMatrix(2, 2)
      M(2, 3) = domain.pointSet.origin(2)
      M(3, 3) = 1

      // the header
      volume.header.setDatatype(ScalarDataType.fromType[S].niftiId)
      volume.header.qform_code = 0
      volume.header.sform_code = 2 // TODO check me that this is right

      val data = M.t.toDenseVector.toArray.map(_.toFloat)

      volume.header.srow_x = data.take(4)
      volume.header.srow_y = data.slice(4, 8)
      volume.header.srow_z = data.slice(8, 12)
      volume.header.pixdim(1) = domain.pointSet.spacing(0).toFloat
      volume.header.pixdim(2) = domain.pointSet.spacing(1).toFloat
      volume.header.pixdim(3) = domain.pointSet.spacing(2).toFloat

      volume.write(file.getAbsolutePath)
    }
  }

  def writeVTK[D: NDSpace: CanConvertToVtk, S: Scalar: ClassTag](img: DiscreteImage[D, S], file: File): Try[Unit] = {

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
