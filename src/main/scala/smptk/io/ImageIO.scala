package smptk
package io

import image.ScalarPixel
import image.Image._
import scala.language.higherKinds
import scala.util.Try
import scala.util.Failure
import image.DiscreteScalarImage2D
import java.io.File
import smptk.image.DiscreteScalarImage2D
import smptk.image.CoordVector
import smptk.image.DiscreteScalarImage
import scala.util.Success
import smptk.image.DiscreteImageDomain2D
import reflect.runtime.universe.{ TypeTag, typeOf }
import smptk.image.Geometry.CoordVector2D
import smptk.image.DiscreteScalarImage2D
import java.io.IOException
import smptk.image.DiscreteImage
import scala.reflect.ClassTag
import smptk.image.DiscreteImageDomain3D
import smptk.image.DiscreteScalarImage3D
import smptk.image.DiscreteImageDomain1D
import smptk.image.DiscreteScalarImage1D

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

  def read1DScalarImage[Scalar: ScalarPixel: TypeTag](f: File): Try[DiscreteScalarImage1D[Scalar]] = {

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
              val domain = DiscreteImageDomain1D(imageData.origin(0).toFloat, imageData.spacing(0).toFloat,imageData.size(0).toInt)  
              Success(DiscreteScalarImage1D(domain, imageData.data))
            }
          }
        }
      }
      case _ => Failure(new Exception("Unknown file type received" + f.getAbsolutePath()))
    }
  }

  def read3DScalarImage[Scalar: ScalarPixel: TypeTag](f: File): Try[DiscreteScalarImage3D[Scalar]] = {

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
                (imageData.origin(0).toFloat, imageData.origin(1).toFloat, imageData.origin(2).toFloat),
                (imageData.spacing(0).toFloat, imageData.spacing(1).toFloat, imageData.spacing(2).toFloat),
                (imageData.size(0).toInt, imageData.size(1).toInt, imageData.size(2).toInt))
              Success(DiscreteScalarImage3D(domain, imageData.data))
            }
          }
        }
      }
      case _ => Failure(new Exception("Unknown file type received" + f.getAbsolutePath()))
    }
  }

  def read2DScalarImage[Scalar: ScalarPixel: TypeTag](f: File): Try[DiscreteScalarImage2D[Scalar]] = {

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
                (imageData.origin(0).toFloat, imageData.origin(1).toFloat),
                (imageData.spacing(0).toFloat, imageData.spacing(1).toFloat),
                (imageData.size(0).toInt, imageData.size(1).toInt))
              Success(DiscreteScalarImage2D(domain, imageData.data))
            }
          }
        }
      }
      case _ => Failure(new Exception("Unknown file type received" + f.getAbsolutePath()))
    }
  }

  /**
   *  read image data in ITK's hdf5 format
   *  @tparam Scalar The type of the Scalar elements in the image
   *  @param file The file name
   *
   */

  private def readHDF5[Scalar: TypeTag](file: File): Try[GenericImageData[Scalar]] = {
    val filename = file.getAbsolutePath()
    val h5file = HDF5Utils.openFileForReading(file)

    def pixelDimensionality(dims: Array[Long], dataDims: IndexedSeq[Long]): Int = {
      if (dims.length == dataDims.length) 1 else dataDims.last.toInt
    }

    val genericImageData = for {
      directions <- h5file.readNDArray[Double]("/ITKImage/0/Directions")
      voxelType <- h5file.readString("/ITKImage/0/VoxelType")
      dims <- h5file.readArray[Long]("/ITKImage/0/Dimension")
      origin <- h5file.readArray[Double]("/ITKImage/0/Origin")
      spacing <- h5file.readArray[Double]("/ITKImage/0/Spacing")
      voxelData <- readAndCheckVoxelData[Scalar](h5file, voxelType)
    } yield GenericImageData(origin, spacing, dims, pixelDimensionality(dims, voxelData.dims), voxelType, voxelData.data)

    h5file.close()

    genericImageData
  }

  def writeImage[CV[A] <: CoordVector[A], Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[CV, Scalar], file: File): Try[Unit] = {
    val filename = file.getAbsolutePath()
    filename match {
      case f if f.endsWith(".h5") => writeHDF5(img, file)
      case _ => {
        Failure(new IOException("Unknown file type received" + filename))
      }
    }
  }

  private def writeHDF5[CV[A] <: CoordVector[A], Scalar: TypeTag: ClassTag](img: DiscreteImage[CV, Scalar], file: File): Try[Unit] = {

    val maybeVoxelType = scalarTypeToString[Scalar]()
    if (maybeVoxelType.isEmpty) {
      return Failure(new Exception(s"invalid voxeltype " + typeOf[Scalar]))
    }
    val voxelType = maybeVoxelType.get

    val h5file = HDF5Utils.createFile(file)

    // append the number of components to the image dimensionality. 
    // The data of an image of size m x n will be saved as an array of dims n x m x d, 
    // where d is the number of components 
    // (note that here the dimensions of the voxelArray are reversed compared the the
    // vector dims that is stored in the field Dimensions. This is the convention of the itk implementation
    // which we follow)
    var voxelArrayDim = img.domain.size.toArray.reverse.map(_.toLong)

    if (img.pixelDimensionality > 1)
      voxelArrayDim = voxelArrayDim ++ Vector[Long](img.pixelDimensionality)

    // TODO directions are currently ignore. This should not be
    val directions = NDArray[Double](
      Vector[Long](img.domain.dimensionality, img.domain.dimensionality),
      img.domain.directions)

    val maybeError: Try[Unit] = for {
      _ <- h5file.writeNDArray("/ITKImage/0/Directions", directions)
      _ <- h5file.writeArray("/ITKImage/0/Dimension", img.domain.size.toArray.map(_.toLong))
      _ <- h5file.writeArray("/ITKImage/0/Origin", img.domain.origin.toArray.map(_.toDouble))
      _ <- h5file.writeArray("/ITKImage/0/Spacing", img.domain.spacing.toArray.map(_.toDouble))
      _ <- h5file.writeNDArray("/ITKImage/0/VoxelData", NDArray(voxelArrayDim, img.pixelValues.toArray))
      _ <- h5file.createGroup("/ITKImage/0/MetaData")
      _ <- h5file.writeString("/ITKVersion", "4.2.0") // we don't need it - ever
      _ <- h5file.writeString("/HDFVersion", HDF5Utils.hdf5Version)
      _ <- h5file.writeString("/ITKImage/0/VoxelType", voxelType)

    } yield { () } // if everything is okay, we have a Unit type and no error here
    h5file.close()
    maybeError
  }

  private def scalarTypeToString[Scalar: TypeTag](): Option[String] = {
    typeOf[Scalar] match {
      case t if t =:= typeOf[Float] => Some("FLOAT")
      case t if t =:= typeOf[Short] => Some("SHORT")
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

