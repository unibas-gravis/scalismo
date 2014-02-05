package org.statismo.stk.core
package io

import scala.util.Try
import scala.util.Failure
import image.DiscreteScalarImage2D
import java.io.File
import org.statismo.stk.core.image.DiscreteScalarImage2D
import org.statismo.stk.core.image.DiscreteScalarImage
import scala.util.Success
import org.statismo.stk.core.image.{ DiscreteImageDomain1D, DiscreteImageDomain2D, DiscreteImageDomain3D }
import reflect.runtime.universe.{ TypeTag, typeOf }
import org.statismo.stk.core.image.DiscreteScalarImage2D
import java.io.IOException
import org.statismo.stk.core.image.DiscreteImage
import scala.reflect.ClassTag
import org.statismo.stk.core.image.DiscreteScalarImage3D
import org.statismo.stk.core.image.DiscreteScalarImage1D
import org.statismo.stk.core.geometry._
import vtk.vtkStructuredPointsReader
import org.statismo.stk.core.utils.ImageConversion
import vtk.vtkImageWriter
import vtk.vtkImageData
import vtk.vtkStructuredPointsWriter
import vtk.vtkStructuredPoints
import org.statismo.stk.core.common.ScalarValue
import org.statismo.stk.core.image.DiscreteScalarImage3D
import niftijio.NiftiVolume
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.image.Interpolation
import org.statismo.stk.core.image.Resample
import org.statismo.stk.core.image.DiscreteScalarImage3D
import niftijio.NiftiHeader

/**
 * WARNING! WE ARE USING RAS COORDINATE SYSTEM
 */

object ImageIO {

  private case class GenericImageData[Scalar](
    val origin: Array[Double],
    val spacing: Array[Double],
    val size: Array[Long],
    val pixelDimensionality: Int,
    val voxelType: String,
    val data: Array[Scalar]) {
    def hasDimensionality(dim: Int): Boolean = {
      origin.size == dim &&
        spacing.size == dim &&
        size.size == dim
    }
  }

  def read1DScalarImage[Scalar: ScalarValue: TypeTag](f: File): Try[DiscreteScalarImage1D[Scalar]] = {

    f match {
      case f if f.getAbsolutePath().endsWith(".h5") => {

        val imageDataOrFailure = readHDF5[Scalar](f)
        imageDataOrFailure.flatMap { imageData =>
          {
            if (imageData.hasDimensionality(1) == false) {
              Failure(new Exception(s"wrong dimensionality in the image data"))
            } else if (imageData.pixelDimensionality != 1) {
              Failure(new Exception("wrong pixel dimensionality in image data"))
            } else {

              val domain = DiscreteImageDomain1D(Point1D(imageData.origin(0).toFloat), Vector1D(imageData.spacing(0).toFloat), Index1D(imageData.size(0).toInt))
              Success(DiscreteScalarImage1D(domain, imageData.data))
            }
          }
        }
      }
      case _ => Failure(new Exception("Unknown file type received" + f.getAbsolutePath()))
    }
  }

  def read3DScalarImage[Scalar: ScalarValue: TypeTag: ClassTag](f: File): Try[DiscreteScalarImage3D[Scalar]] = {

    f match {
      case f if f.getAbsolutePath().endsWith(".h5") => {

        val imageDataOrFailure = readHDF5[Scalar](f)
        imageDataOrFailure.flatMap { imageData =>
          {
            if (imageData.hasDimensionality(3) == false) {
              Failure(new Exception(s"wrong dimensionality in the image data"))
            } else if (imageData.pixelDimensionality != 1) {
              Failure(new Exception("wrong pixel dimensionality in image data"))
            } else {
              val domain = DiscreteImageDomain3D(
                Point3D(imageData.origin(0).toFloat, imageData.origin(1).toFloat, imageData.origin(2).toFloat),
                Vector3D(imageData.spacing(0).toFloat, imageData.spacing(1).toFloat, imageData.spacing(2).toFloat),
                Index3D(imageData.size(0).toInt, imageData.size(1).toInt, imageData.size(2).toInt))

              Success(DiscreteScalarImage3D(domain, imageData.data))
            }
          }
        }
      }
      case f if f.getAbsolutePath().endsWith(".vtk") => {
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException("Failed to read vtk file ${f.getAbsolutePath()}. " +
            "(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsTo3DScalarImage[Scalar](sp)
        reader.Delete()
        sp.Delete()
        img
      }
      case f if (f.getAbsolutePath().endsWith(".nii") || f.getAbsolutePath().endsWith(".nia")) => {
        readNifti[Scalar](f)
      }
      case _ => Failure(new Exception("Unknown file type received" + f.getAbsolutePath()))
    }
  }

  def read2DScalarImage[Scalar: ScalarValue: TypeTag](f: File): Try[DiscreteScalarImage2D[Scalar]] = {

    f match {
      case f if f.getAbsolutePath().endsWith(".h5") => {

        val imageDataOrFailure = readHDF5[Scalar](f)
        imageDataOrFailure.flatMap { imageData =>
          {
            if (imageData.hasDimensionality(2) == false) {
              Failure(new Exception("wrong dimensionality in the image data "))
            } else if (imageData.pixelDimensionality != 1) {
              Failure(new Exception("wrong pixel dimensionality in image data"))
            } else {
              val domain = DiscreteImageDomain2D(
                Point2D(imageData.origin(0).toFloat, imageData.origin(1).toFloat),
                Vector2D(imageData.spacing(0).toFloat, imageData.spacing(1).toFloat),
                Index2D(imageData.size(0).toInt, imageData.size(1).toInt))
              Success(DiscreteScalarImage2D(domain, imageData.data))
            }
          }
        }
      }
      case f if f.getAbsolutePath().endsWith(".vtk") => {
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException("Failed to read vtk file ${f.getAbsolutePath()}. " +
            "(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsTo2DScalarImage[Scalar](sp)
        reader.Delete()
        sp.Delete()
        img
      }

      case _ => Failure(new Exception("Unknown file type received" + f.getAbsolutePath()))
    }
  }

  private def readNifti[Scalar: ScalarValue: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage3D[Scalar]] = {

    val scalarConv = implicitly[ScalarValue[Scalar]]
    val volume = NiftiVolume.read(file.getAbsolutePath());

    val nx = volume.header.dim(1);
    val ny = volume.header.dim(2);
    val nz = volume.header.dim(3);
    var dim = volume.header.dim(4);

    if (dim == 0)
      dim = 1

    for {
      (transVoxelToWorld, transWorldToVoxel) <- computeNiftiWorldToVoxelTransforms(volume)
    } yield {

      // the 8 corners of the box
      val c1 = transVoxelToWorld(Point3D(0, 0, 0))
      val c2 = transVoxelToWorld(Point3D(0, 0, nz - 1))
      val c3 = transVoxelToWorld(Point3D(0, ny - 1, 0))
      val c4 = transVoxelToWorld(Point3D(0, ny - 1, nz - 1))
      val c5 = transVoxelToWorld(Point3D(nx - 1, 0, 0))
      val c6 = transVoxelToWorld(Point3D(nx - 1, 0, nz - 1))
      val c7 = transVoxelToWorld(Point3D(nx - 1, ny - 1, 0))
      val c8 = transVoxelToWorld(Point3D(nx - 1, ny - 1, nz - 1))

      val voxelDataVTK = for (d <- 0 until dim; k <- 0 until nz; j <- 0 until ny; i <- 0 until nx) yield volume.data(i)(j)(k)(d);

      // we create an image from the raw voxel data, which we can then transform using our transformation machinery to its world coordinates.
      val unitDomain = DiscreteImageDomain3D(Point3D(0, 0, 0), Vector3D(1, 1, 1), Index3D(nx, ny, nz))
      val img = DiscreteScalarImage3D[Scalar](unitDomain, voxelDataVTK.map(v => scalarConv.fromDouble(v)).toArray)

      val corners = IndexedSeq(c1, c2, c3, c4, c5, c6, c7, c8)

      val newOrigin = Point3D(corners.map(c => c(0)).min.toFloat, corners.map(c => c(1)).min.toFloat, corners.map(c => c(2)).min.toFloat)
      val newExtent = Point3D(corners.map(c => c(0)).max.toFloat, corners.map(c => c(1)).max.toFloat, corners.map(c => c(2)).max.toFloat)

      val cimg = Interpolation.interpolate(img, 0)
      val newSpacing = Vector3D((newExtent - newOrigin)(0) / nx, (newExtent - newOrigin)(1) / ny, (newExtent - newOrigin)(2) / nz)
      val newDomain = DiscreteImageDomain3D(newOrigin, newSpacing, Index3D(nx, ny, nz))
      Resample.sample[Scalar](cimg.compose(transWorldToVoxel), newDomain, 0f)
    }

  }

  /**
   * returns transformations from voxel to World coordinates and its inverse
   */
  private[this] def computeNiftiWorldToVoxelTransforms(volume: NiftiVolume): Try[(Transformation[ThreeD], Transformation[ThreeD])] = {

    val nx = volume.header.dim(1);
    val ny = volume.header.dim(2);
    val nz = volume.header.dim(3);
    var dim = volume.header.dim(4);

    if (dim == 0)
      dim = 1

    // check this page http://brainder.org/2012/09/23/the-nifti-file-format/
    // for details about the nifty format
    if (volume.header.sform_code == 0) return Failure(new IOException("currently we can only read nifty format with sform_code > 0"))

    val affineTransMatrix = DenseMatrix.create(4, 4, volume.header.sform_to_mat44().flatten).t

    val t: Transformation[ThreeD] = new Transformation[ThreeD] {
      def apply(x: Point[ThreeD]) = {
        val xh = DenseVector(x(0), x(1), x(2), 1.0)
        val t = affineTransMatrix * xh
        Point3D(t(0).toFloat, t(1).toFloat, t(2).toFloat)
      }
      override def takeDerivative(x: Point[ThreeD]): Matrix3x3 = ???
    }

    val affineTransMatrixInv = breeze.linalg.inv(affineTransMatrix)
    val tinv: Transformation[ThreeD] = new Transformation[ThreeD] {
      def apply(x: Point[ThreeD]) = {
        val xh = DenseVector(x(0), x(1), x(2), 1.0)
        val t = affineTransMatrixInv * xh
        Point3D(t(0).toFloat, t(1).toFloat, t(2).toFloat)
      }
      override def takeDerivative(x: Point[ThreeD]): Matrix3x3 = ???
    }

    Success(t, tinv)
  }

  /**
   *  read image data in ITK's hdf5 format
   *  @tparam Scalar The type of the Scalar elements in the image
   *  @param file The file name
   *
   */

  private def readHDF5[Scalar: TypeTag](file: File): Try[GenericImageData[Scalar]] = {
    val filename = file.getAbsolutePath()

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
      _ <- Try { h5file.close }
    } yield GenericImageData(origin, spacing, dims, pixelDimensionality(dims, voxelData.dims), voxelType, voxelData.data)

    genericImageData
  }

  def writeImage[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage1D[Scalar], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  def writeImage[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage2D[Scalar], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case f if f.endsWith(".vtk") => writeVTK(img, file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  def writeImage[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage3D[Scalar], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case f if f.endsWith(".vtk") => writeVTK(img, file)
      case f if f.endsWith(".nii") || f.endsWith(".nia") => writeNifti(img, file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  private[this] def writeNifti[Scalar: ScalarValue: TypeTag: ClassTag](img: DiscreteScalarImage3D[Scalar], file: File): Try[Unit] = {
    val scalarConv = implicitly[ScalarValue[Scalar]]
    
    val domain = img.domain
    val size = domain.size
    val dim = 1;

    Try {

      val volume = new NiftiVolume(size(0), size(1), size(2), dim)

      // the data
      for (d <- 0 until dim) {
        val d1 = for (k <- 0 until size(2)) {
          val d2 = for (j <- 0 until size(1)) {
            val d3 = for (i <- 0 until size(0)) {
              volume.data(i)(j)(k)(d) = scalarConv.toDouble(img(Index3D(i, j, k)))
            }
          }
        }
      }

      // the header
    //  val header = new NiftiHeader()
      volume.header.setDatatype(niftyDataTypeFromScalar[Scalar])
      volume.header.qform_code = 0
      volume.header.sform_code = 2 // TODO check me that this is right
      volume.header.srow_x(0) = domain.spacing(0); volume.header.srow_x(1) =  0f; volume.header.srow_x(2)= 0f; volume.header.srow_x(3) = domain.origin(0)
      volume.header.srow_y(0) = 0f; volume.header.srow_y(1) =  domain.spacing(1); volume.header.srow_y(2)= 0f; volume.header.srow_y(3) = domain.origin(1)
      volume.header.srow_z(0) = 0f; volume.header.srow_z(1) = 0f; volume.header.srow_z(2)=  domain.spacing(2);; volume.header.srow_z(3) = domain.origin(2)
      volume.header.pixdim(1) = domain.spacing(0)
      volume.header.pixdim(2) = domain.spacing(1)
      volume.header.pixdim(3) = domain.spacing(2)
      volume.write(file.getAbsolutePath())
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
    writer.SetFileName(file.getAbsolutePath())
    writer.Update()
    val errorCode = writer.GetErrorCode()
    if (errorCode != 0) {
      return Failure(new IOException(s"Error writing vtk file ${file.getAbsolutePath()} (error code $errorCode"))
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
      voxelArrayDim = voxelArrayDim ++ Vector[Long](img.valueDimensionality)

    // TODO directions are currently ignore. This should not be
    val directions = NDArray[Double](
      Vector[Long](img.domain.dimensionality, img.domain.dimensionality),
      img.domain.directions)

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
      _ <- Try { h5file.close() }
    } yield { () } // if everything is okay, we have a Unit type and no error here
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

} // end of enclosing object

