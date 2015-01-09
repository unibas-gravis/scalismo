package org.statismo.stk.core
package io

import scala.util.Try
import scala.util.Failure
import java.io.File
import scala.util.Success
import org.statismo.stk.core.image.DiscreteImageDomain
import reflect.runtime.universe.{ TypeTag, typeOf }
import org.statismo.stk.core.image.DiscreteScalarImage2D
import java.io.IOException
import org.statismo.stk.core.image.DiscreteImage
import scala.reflect.ClassTag
import org.statismo.stk.core.image.DiscreteScalarImage1D
import org.statismo.stk.core.geometry._
import vtk.vtkStructuredPointsReader
import org.statismo.stk.core.utils.ImageConversion
import vtk.vtkStructuredPointsWriter
import vtk.vtkStructuredPoints
import org.statismo.stk.core.common.ScalarValue
import niftijio.NiftiVolume
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.image.DiscreteScalarImage3D
import org.statismo.stk.core.registration.LandmarkRegistration
import org.statismo.stk.core.registration.AnisotropicScalingSpace
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformationSpace

import org.statismo.stk.core.image.{ DiscreteImageDomain1D, DiscreteImageDomain2D, DiscreteImageDomain3D }
import reflect.runtime.universe.{ TypeTag, typeOf }
import org.statismo.stk.core.registration.AnisotropicScalingSpace
/**
 * WARNING! WE ARE USING RAS COORDINATE SYSTEM
 */

object ImageIO {

  private case class GenericImageData[Scalar](

    origin: Array[Double],
    spacing: Array[Double],
    size: Array[Long],
    pixelDimensionality: Int,
    voxelType: String,
    data: Array[Scalar]) {

    def hasDimensionality(dim: Int): Boolean = {
      origin.size == dim &&
        spacing.size == dim &&
        size.size == dim
    }
  }

  def read1DScalarImage[Scalar: ScalarValue: TypeTag](file: File): Try[DiscreteScalarImage1D[Scalar]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".h5") =>

        val imageDataOrFailure = readHDF5[Scalar](f)
        imageDataOrFailure.flatMap {

          imageData =>
            {
              if (!imageData.hasDimensionality(1)) {
                Failure(new Exception(s"wrong dimensionality in the image data"))
              } else if (imageData.pixelDimensionality != 1) {
                Failure(new Exception("wrong pixel dimensionality in image data"))
              } else {

                val domain = DiscreteImageDomain[_1D](Point(imageData.origin(0).toFloat), Vector(imageData.spacing(0).toFloat), Index(imageData.size(0).toInt))
                Success(DiscreteScalarImage1D(domain, imageData.data))
              }
            }
        }
      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  def read3DScalarImage[Scalar: ScalarValue: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage3D[Scalar]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".h5") =>

        val imageDataOrFailure = readHDF5[Scalar](f)
        imageDataOrFailure.flatMap {

          imageData =>
            {
              if (!imageData.hasDimensionality(3)) {
                Failure(new Exception(s"wrong dimensionality in the image data"))
              } else if (imageData.pixelDimensionality != 1) {
                Failure(new Exception("wrong pixel dimensionality in image data"))
              } else {
                val domain = DiscreteImageDomain[_3D](
                  Point(imageData.origin(0).toFloat, imageData.origin(1).toFloat, imageData.origin(2).toFloat),
                  Vector(imageData.spacing(0).toFloat, imageData.spacing(1).toFloat, imageData.spacing(2).toFloat),
                  Index(imageData.size(0).toInt, imageData.size(1).toInt, imageData.size(2).toInt))

                Success(DiscreteScalarImage3D(domain, imageData.data))

              }
            }
        }
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException(s"Failed to read vtk file ${f.getAbsolutePath}. " +
            "(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsTo3DScalarImage[Scalar](sp)
        reader.Delete()
        sp.Delete()
        img
      case f if f.getAbsolutePath.endsWith(".nii") || f.getAbsolutePath.endsWith(".nia") =>
        readNifti[Scalar](f)
      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  def read2DScalarImage[Scalar: ScalarValue: TypeTag](file: File): Try[DiscreteScalarImage2D[Scalar]] = {

    file match {
      case f if f.getAbsolutePath.endsWith(".h5") =>

        val imageDataOrFailure = readHDF5[Scalar](f)
        imageDataOrFailure.flatMap {

          imageData =>
            {
              if (!imageData.hasDimensionality(2)) {
                Failure(new Exception("wrong dimensionality in the image data "))
              } else if (imageData.pixelDimensionality != 1) {
                Failure(new Exception("wrong pixel dimensionality in image data"))
              } else {
                val domain = DiscreteImageDomain[_2D](
                  Point(imageData.origin(0).toFloat, imageData.origin(1).toFloat),
                  Vector(imageData.spacing(0).toFloat, imageData.spacing(1).toFloat),
                  Index(imageData.size(0).toInt, imageData.size(1).toInt))
                Success(DiscreteScalarImage2D(domain, imageData.data))
              }

            }
        }
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException("Failed to read vtk file ${file.getAbsolutePath()}. " +
            "(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsTo2DScalarImage[Scalar](sp)
        reader.Delete()
        sp.Delete()
        img

      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  private def readNifti[Scalar: ScalarValue: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage3D[Scalar]] = {

    val scalarConv = implicitly[ScalarValue[Scalar]]

    for {

      volume <- FastReadOnlyNiftiVolume.read(file.getAbsolutePath)
      pair <- computeNiftiWorldToVoxelTransforms(volume)
    } yield {
      val (transVoxelToWorld, transWorldToVoxel) = pair

      val nx = volume.header.dim(1)
      val ny = volume.header.dim(2)
      val nz = volume.header.dim(3)
      var dim = volume.header.dim(4)

      if (dim == 0)
        dim = 1

      /* figure out the anisotropic scaling factor */
      val s = volume.header.pixdim
      val spacing = DenseVector(s(1), s(2), s(3))

      val anisotropicScaling = new AnisotropicScalingSpace[_3D].transformForParameters(spacing)

      /* get a rigid registration by mapping a few points */
      val origPs = List(Point(0, 0, nz), Point(0, ny, 0), Point(0, ny, nz), Point(nx, 0, 0), Point(nx, 0, nz), Point(nx, ny, 0), Point(nx, ny, nz))
      val scaledPS = origPs.map(anisotropicScaling)
      val imgPs = origPs.map(transVoxelToWorld)

      val rigidReg = LandmarkRegistration.rigid3DLandmarkRegistration((scaledPS zip imgPs).toIndexedSeq)
      val transform = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(DenseVector(rigidReg.parameters.data ++ spacing.data))

      val newDomain = DiscreteImageDomain[_3D](Index(nx, ny, nz), transform)
      DiscreteScalarImage3D[Scalar](newDomain, volume.dataArray.map(v => scalarConv.fromDouble(v)))

    }
  }

  /**
   * returns transformations from voxel to World coordinates and its inverse
   */

  private[this] def computeNiftiWorldToVoxelTransforms(volume: FastReadOnlyNiftiVolume): Try[(Transformation[_3D], Transformation[_3D])] = {
    var dim = volume.header.dim(4)

    if (dim == 0)
      dim = 1

    // check this page http://brainder.org/2012/09/23/the-nifti-file-format/
    // for details about the nifty format

    if (volume.header.sform_code == 0 && volume.header.qform_code == 0)
      return Failure(new IOException("cannot read nifti with both qform and sform codes set to 0"))

    val affineTransMatrix = if (volume.header.sform_code == 0 && volume.header.qform_code > 0)
      DenseMatrix.create(4, 4, volume.header.qform_to_mat44.flatten).t
    else
      DenseMatrix.create(4, 4, volume.header.sformArray).t

    // flip scaling to account for RAS coordinates 
    affineTransMatrix(0, 0) = -affineTransMatrix(0, 0)
    affineTransMatrix(1, 1) = -affineTransMatrix(1, 1)

    //also flip origin (translation params) 
    affineTransMatrix(0, 3) = -affineTransMatrix(0, 3)
    affineTransMatrix(1, 3) = -affineTransMatrix(1, 3)

    val t = new Transformation[_3D] {
      def apply(x: Point[_3D]) = {
        val xh = DenseVector(x(0), x(1), x(2), 1.0)
        val t: DenseVector[Double] = affineTransMatrix * xh
        Point(t(0).toFloat, t(1).toFloat, t(2).toFloat)
      }
      //override def takeDerivative(x: Point[ThreeD]): Matrix3x3 = ???
    }

    val affineTransMatrixInv: DenseMatrix[Double] = breeze.linalg.inv(affineTransMatrix)
    val tinv = new Transformation[_3D] {
      def apply(x: Point[_3D]) = {
        val xh: DenseVector[Double] = DenseVector(x(0), x(1), x(2), 1.0)
        val t: DenseVector[Float] = (affineTransMatrixInv * xh).map(_.toFloat)
        Point(t(0), t(1), t(2))
      }
    }

    Success((t, tinv))
  }

  /**
   * read image data in ITK's hdf5 format
   * @tparam Scalar The type of the Scalar elements in the image
   * @param file The file name
   *
   */

  private def readHDF5[Scalar: TypeTag](file: File): Try[GenericImageData[Scalar]] = {
    def pixelDimensionality(dims: Array[Long], dataDims: IndexedSeq[Long]): Int = {
      if (dims.length == dataDims.length) 1 else dataDims.last.toInt
    }

    val genericImageData = for {
      h5file <- HDF5Utils.openFileForReading(file)
      directions <- h5file.readNDArray[Double]("/ITKImage/0/Directions")
      voxelType <- h5file.readString("/ITKImage/0/VoxelType")
      dims <- h5file.readArray[Long]("/ITKImage/0/Dimension")
      origin <- h5file.readArray[Double]("/ITKImage/0/Origin")
      spacing <- h5file.readArray[Double]("/ITKImage/0/Spacing")
      voxelData <- readAndCheckVoxelData[Scalar](h5file, voxelType)
      _ <- Try {
        h5file.close()
      }
    } yield GenericImageData(origin, spacing, dims, pixelDimensionality(dims, voxelData.dims), voxelType, voxelData.data)

    genericImageData
  }

  def writeImage[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage1D[Scalar], file: File): Try[Unit] = {

    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeImage[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage2D[Scalar], file: File): Try[Unit] = {

    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case f if f.endsWith(".vtk") => writeVTK(img, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  def writeImage[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage3D[Scalar], file: File): Try[Unit] = {

    val filename = file.getAbsolutePath
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case f if f.endsWith(".vtk") => writeVTK(img, file)
      case f if f.endsWith(".nii") || f.endsWith(".nia") => writeNifti(img, file)
      case _ =>
        Failure(new IOException("Unknown file type received" + filename))
    }
  }

  private[this] def writeNifti[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage3D[Scalar], file: File): Try[Unit] = {
    val scalarConv = implicitly[ScalarValue[Scalar]]

    val domain = img.domain
    val size = domain.size
    val dim = 1

    Try {

      val volume = new NiftiVolume(size(0), size(1), size(2), dim)

      // the data

      for (d <- 0 until dim; k <- 0 until size(2); j <- 0 until size(1); i <- 0 until size(0)) {
        volume.data(i)(j)(k)(d) = scalarConv.toDouble(img(Index(i, j, k)))

      }

      // params : (Translation++rotation++anisScaling)
      val anisotropicTransformParams = img.domain.indexToPhysicalCoordinateTransform.parameters.data
      val translationParams = anisotropicTransformParams.take(3)
      val rotationParams = anisotropicTransformParams.drop(3).take(3)
      val scalingParams = anisotropicTransformParams.drop(6).take(3)

      if (breeze.linalg.norm(DenseVector(scalingParams.map(Math.abs)) - domain.spacing.toBreezeVector) > 0.01f || breeze.linalg.norm(DenseVector(translationParams) - domain.origin.toBreezeVector) > 0.01f)
        return Failure(new Exception("NiftiIO: indicated anistotropic similarity transform parameters mismatch with the image domain. params : " + scalingParams.deep + " domain spacing : " + domain.spacing))

      val alpha = rotationParams(0)
      val beta = rotationParams(1)
      val gamma = rotationParams(2)

      val ca = Math.cos(alpha); val sa = Math.sin(alpha);
      val cb = Math.cos(beta); val sb = Math.sin(beta);
      val cg = Math.cos(gamma); val sg = Math.sin(gamma);

      /**
       * Here to force the RAS view, we invert the first and second component of the anisotropic scaling
       * We also flip the first two components of the translation to the origin
       */

      val M = DenseMatrix.zeros[Double](4, 4)
      M(0, 0) = cb * cg * (domain.spacing(0) * -1f); M(0, 1) = -cb * sg; M(0, 2) = sb; M(0, 3) = -domain.origin(0)
      M(1, 0) = ca * sg + sa * sb * cg; M(1, 1) = -1f * (ca * cg - (sa * sb * sg)) * domain.spacing(1); M(1, 2) = -sa * cb; M(1, 3) = -domain.origin(1)
      M(2, 0) = sa * sg - ca * sb * cg; M(2, 1) = (sa * cg + (ca * sb * sg)); M(2, 2) = domain.spacing(2) * (ca * cb); M(2, 3) = domain.origin(2)
      M(3, 3) = 1

      // the header
      //  val header = new NiftiHeader()
      volume.header.setDatatype(niftyDataTypeFromScalar[Scalar])
      volume.header.qform_code = 0
      volume.header.sform_code = 2 // TODO check me that this is right

      volume.header.srow_x = M.t.toDenseVector.data.take(4).map(_.toFloat)
      volume.header.srow_y = M.t.toDenseVector.data.drop(4).take(4).map(_.toFloat)
      volume.header.srow_z = M.t.toDenseVector.data.drop(8).take(4).map(_.toFloat)
      volume.header.pixdim(1) = domain.spacing(0)
      volume.header.pixdim(2) = domain.spacing(1)
      volume.header.pixdim(3) = domain.spacing(2)

      volume.write(file.getAbsolutePath)
    }
  }

  private[this] def niftyDataTypeFromScalar[Scalar: ScalarValue: TypeTag: ClassTag]: Short = {

    typeOf[Scalar] match {
      case t if t =:= typeOf[Char] => 2
      case t if t <:< typeOf[Short] => 4
      case t if t <:< typeOf[Int] => 8
      case t if t <:< typeOf[Float] => 16
      case t if t <:< typeOf[Double] => 64
      case _ => throw new Throwable(s"Unsupported datatype ${typeOf[Scalar]}")
    }
  }

  private def writeVTK[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage2D[Scalar], file: File): Try[Unit] = {

    val imgVtk = ImageConversion.image2DTovtkStructuredPoints(img)
    writeVTKInternal(imgVtk, file)
  }

  private def writeVTK[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage3D[Scalar], file: File): Try[Unit] = {
    val imgVtk = ImageConversion.image3DTovtkStructuredPoints(img)
    writeVTKInternal(imgVtk, file)
  }

  private def writeVTKInternal(imgVtk: vtkStructuredPoints, file: File): Try[Unit] = {
    val writer = new vtkStructuredPointsWriter()
    writer.SetInputData(imgVtk)
    writer.SetFileName(file.getAbsolutePath)
    writer.SetFileTypeToBinary()
    writer.Update()
    val errorCode = writer.GetErrorCode()
    if (errorCode != 0) {
      Failure(new IOException(s"Error writing vtk file ${file.getAbsolutePath} (error code $errorCode"))
    } else {
      Success(())
    }
  }

  private def writeHDF5[D <: Dim, Scalar: TypeTag: ClassTag](img: DiscreteImage[D, Scalar], file: File): Try[Unit] = {

    val maybeVoxelType = scalarTypeToString[Scalar]()
    if (maybeVoxelType.isEmpty) {
      return Failure(new Exception(s"invalid voxeltype " + typeOf[Scalar]))
    }
    val voxelType = maybeVoxelType.get

    // append the number of components to the image dimensionality. 
    // The data of an image of size m x n will be saved as an array of dims n x m x d, 
    // where d is the number of components 
    // (note that here the dimensions of the voxelArray are reversed compared the the
    // vector dims that is stored in the field Dimensions. This is the convention of the itk implementation
    // which we follow)
    var voxelArrayDim = img.domain.size.data.reverse.map(_.toLong)

    if (img.valueDimensionality > 1)
      voxelArrayDim = voxelArrayDim ++ IndexedSeq[Long](img.valueDimensionality)

    val directions = NDArray[Double](
      IndexedSeq[Long](img.domain.size.dimensionality, img.domain.size.dimensionality),
      img.domain.directions.data.map(_.toDouble))

    val maybeError: Try[Unit] = for {
      h5file <- HDF5Utils.createFile(file)
      _ <- h5file.writeNDArray("/ITKImage/0/Directions", directions)
      _ <- h5file.writeArray("/ITKImage/0/Dimension", img.domain.size.data.map(_.toLong))
      _ <- h5file.writeArray("/ITKImage/0/Origin", img.domain.origin.data.map(_.toDouble))
      _ <- h5file.writeArray("/ITKImage/0/Spacing", img.domain.spacing.data.map(_.toDouble))
      _ <- h5file.writeNDArray("/ITKImage/0/VoxelData", NDArray(voxelArrayDim, img.values.toArray))
      _ <- h5file.createGroup("/ITKImage/0/MetaData")
      _ <- h5file.writeString("/ITKVersion", "4.2.0") // we don't need it - ever
      _ <- h5file.writeString("/HDFVersion", HDF5Utils.hdf5Version)
      _ <- h5file.writeString("/ITKImage/0/VoxelType", voxelType)
      _ <- Try {
        h5file.close()
      }
    } yield {
      ()
    } // if everything is okay, we have a Unit type and no error here
    maybeError
  }

  private def scalarTypeToString[Scalar: TypeTag](): Option[String] = {
    typeOf[Scalar] match {
      case t if t =:= typeOf[Float] => Some("FLOAT")
      case t if t =:= typeOf[Short] => Some("SHORT")
      case t if t =:= typeOf[Double] => Some("DOUBLE")
      case _ => None
    }
  }

  private def readAndCheckVoxelData[Scalar: TypeTag](h5file: HDF5File, voxelType: String): Try[NDArray[Scalar]] = {
    h5file.readNDArray[Scalar]("/ITKImage/0/VoxelData").flatMap(voxelData => {
      val typeString = scalarTypeToString[Scalar]().getOrElse("unknown type")
      if (typeString == voxelType) Success(voxelData)
      else Failure(
        throw new Exception(s"Specified scalar type ($typeString) does not match voxeltype ($voxelType)"))
    })
  }
}

// end of enclosing object

