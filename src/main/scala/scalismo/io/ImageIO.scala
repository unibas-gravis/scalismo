package scalismo.io

import scalismo.common.RealSpace
import scalismo.image.{DiscreteImageDomain, DiscreteScalarImage}
import scalismo.geometry._
import scalismo.registration.{AnisotropicSimilarityTransformationSpace, LandmarkRegistration, AnisotropicScalingSpace, Transformation}
import scalismo.utils.ImageConversion
import scalismo.utils.ImageConversion.CanConvertToVTK
import scala.util.Try
import scala.util.Failure
import java.io.File
import scala.util.Success
import reflect.runtime.universe.{ TypeTag, typeOf }
import java.io.IOException
import scala.reflect.ClassTag
import vtk.vtkStructuredPointsReader
import vtk.vtkStructuredPointsWriter
import vtk.vtkStructuredPoints
import niftijio.NiftiVolume
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import reflect.runtime.universe.{ TypeTag, typeOf }
import spire.math.Numeric

/**
 * Implements methods for reading and writing D-dimensional images  
 * 
 * WARNING! WE ARE USING an LPS COORDINATE SYSTEM
 * 
 * VTK file format does not indicate the orientation of the image. 
 * Therefore, when reading from VTK, we assume that it is in LPS world coordinates. 
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
 * This is done by :    
 *    * mirroring the first two dimensions of the scaling parameters of the affine transform
 *    * mirroring the first two dimensions of the image origin (translation parameters)
 *   
 * The same mirroring is done again when writing an image to the Nifti format.      
 * 
 * 
 * Documentation on orientation :
 * 
 * http://www.grahamwideman.com/gw/brain/orientation/orientterms.htm
 * http://www.slicer.org/slicerWiki/index.php/Coordinate_systems
 * http://brainder.org/2012/09/23/the-nifti-file-format/ 
 * 
 */

object ImageIO {

  trait WriteNifty[D <: Dim] {
    def write[A: Numeric: TypeTag: ClassTag](img: DiscreteScalarImage[D, A], f: File): Try[Unit]
  }

  implicit object DiscreteScalarImage3DNifty extends WriteNifty[_3D] {
    def write[A: Numeric: TypeTag: ClassTag](img: DiscreteScalarImage[_3D, A], f: File): Try[Unit] = {
      writeNifti[A](img, f)
    }
  }

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

  def read1DScalarImage[Scalar: Numeric: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage[_1D, Scalar]] = {

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
                Success(DiscreteScalarImage(domain, imageData.data))
              }
            }
        }
      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  def read3DScalarImage[Scalar: Numeric: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage[_3D, Scalar]] = {

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

                Success(DiscreteScalarImage(domain, imageData.data))

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
            s"(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsToScalarImage[_3D, Scalar](sp)
        reader.Delete()
        sp.Delete()
        img
      case f if f.getAbsolutePath.endsWith(".nii") || f.getAbsolutePath.endsWith(".nia") =>
        readNifti[Scalar](f)
      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  def read2DScalarImage[Scalar: Numeric: ClassTag: TypeTag](file: File): Try[DiscreteScalarImage[_2D, Scalar]] = {

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
                Success(DiscreteScalarImage(domain, imageData.data))
              }

            }
        }
      case f if f.getAbsolutePath.endsWith(".vtk") =>
        val reader = new vtkStructuredPointsReader()
        reader.SetFileName(f.getAbsolutePath)
        reader.Update()
        val errCode = reader.GetErrorCode()
        if (errCode != 0) {
          return Failure(new IOException(s"Failed to read vtk file ${file.getAbsolutePath()}. " +
            s"(error code from vtkReader = $errCode"))
        }
        val sp = reader.GetOutput()
        val img = ImageConversion.vtkStructuredPointsToScalarImage[_2D, Scalar](sp)
        reader.Delete()
        sp.Delete()
        img

      case _ => Failure(new Exception("Unknown file type received" + file.getAbsolutePath))
    }
  }

  private def readNifti[Scalar: Numeric: TypeTag: ClassTag](file: File): Try[DiscreteScalarImage[_3D, Scalar]] = {

    val scalarConv = implicitly[Numeric[Scalar]]

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

      // figure out if the ijk to xyz_RAS transform does mirror: determinant of the linear transform

      val augmentedMatrix = transformMatrixFromNifti(volume).get // get is safe in here 
      val linearTransMatrix = augmentedMatrix(0 to 2, 0 to 2)

      val mirrorScale = (breeze.linalg.det(linearTransMatrix)).signum.toFloat

      val spacing = DenseVector(s(1), s(2), s(3) * mirrorScale)

      val anisotropicScaling = new AnisotropicScalingSpace[_3D].transformForParameters(spacing)

      /* get a rigid registration by mapping a few points */
      val origPs = List(Point(0, 0, nz), Point(0, ny, 0), Point(0, ny, nz), Point(nx, 0, 0), Point(nx, 0, nz), Point(nx, ny, 0), Point(nx, ny, nz))
      val scaledPS = origPs.map(anisotropicScaling)
      val imgPs = origPs.map(transVoxelToWorld)

      val rigidReg = LandmarkRegistration.rigid3DLandmarkRegistration((scaledPS zip imgPs).toIndexedSeq)
      val transform = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(DenseVector(rigidReg.parameters.data ++ spacing.data))

      /* Test that were able to reconstruct the transform */
      val approxErros = (origPs.map(transform) zip imgPs).map { case (o, i) => (o - i).norm }
      if (approxErros.max > 0.001f) throw new Exception("Unable to approximate nifti affine transform wiht anisotropic similarity transform")
      else {
        val newDomain = DiscreteImageDomain[_3D](Index(nx, ny, nz), transform)
        DiscreteScalarImage(newDomain, volume.dataArray.map(v => scalarConv.fromDouble(v)))
      }
    }
  }

  /** returns the augmented matrix of the affine transform from ijk to xyz_RAS */

  private[this] def transformMatrixFromNifti(volume: FastReadOnlyNiftiVolume): Try[DenseMatrix[Double]] = {
    if (volume.header.sform_code == 0 && volume.header.qform_code == 0)
      Failure(new IOException("cannot read nifti with both qform and sform codes set to 0"))
    else {
      if (volume.header.sform_code == 0 && volume.header.qform_code > 0)
        Success(DenseMatrix.create(4, 4, volume.header.qform_to_mat44.flatten).t)
      else
        Success(DenseMatrix.create(4, 4, volume.header.sformArray).t)
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

    transformMatrixFromNifti(volume).map { affineTransMatrix =>

      // flip scaling to account for RAS coordinates 
      affineTransMatrix(0, 0) = -affineTransMatrix(0, 0)
      affineTransMatrix(1, 1) = -affineTransMatrix(1, 1)

      //also flip origin (translation params) 
      affineTransMatrix(0, 3) = -affineTransMatrix(0, 3)
      affineTransMatrix(1, 3) = -affineTransMatrix(1, 3)

      val t = new Transformation[_3D] {
        override val domain = RealSpace[_3D]
        override val f = (x: Point[_3D]) => {
          val xh = DenseVector(x(0), x(1), x(2), 1.0)
          val t: DenseVector[Double] = affineTransMatrix * xh
          Point(t(0).toFloat, t(1).toFloat, t(2).toFloat)
        }
        //override def takeDerivative(x: Point[ThreeD]): Matrix3x3 = ???
      }

      val affineTransMatrixInv: DenseMatrix[Double] = breeze.linalg.inv(affineTransMatrix)
      val tinv = new Transformation[_3D] {
        override val f = (x: Point[_3D]) => {
          val xh: DenseVector[Double] = DenseVector(x(0), x(1), x(2), 1.0)
          val t: DenseVector[Float] = (affineTransMatrixInv * xh).map(_.toFloat)
          Point(t(0), t(1), t(2))
        }
        override val domain = RealSpace[_3D]
      }

      (t, tinv)
    }
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

  def writeNifti[Scalar: Numeric: TypeTag: ClassTag](img: DiscreteScalarImage[_3D, Scalar], file: File): Try[Unit] = {

    val scalarConv = implicitly[Numeric[Scalar]]

    val domain = img.domain
    val size = domain.size
    val dim = 1

    Try {

      val volume = new NiftiVolume(size(0), size(1), size(2), dim)

      // the data

      for (d <- 0 until dim; k <- 0 until size(2); j <- 0 until size(1); i <- 0 until size(0)) {
        volume.data(i)(j)(k)(d) = scalarConv.toDouble(img(Index(i, j, k)))

      }

      
      val M = DenseMatrix.zeros[Double](4, 4)
      M(0, 0) = -1f * domain.spacing(0) * domain.directions(0,0); M(0, 1) = 0; M(0, 2) = 0; M(0, 3) = -domain.origin(0)
      M(1, 0) = 0; M(1, 1) = -1f * domain.spacing(1) * domain.directions(1,1); M(1, 2) = 0; M(1, 3) = -domain.origin(1)
      M(2, 0) = 0; M(2, 1) = 0; M(2, 2) = domain.spacing(2) * domain.directions(2,2); M(2, 3) = domain.origin(2)
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

  private[this] def niftyDataTypeFromScalar[Scalar: Numeric: TypeTag: ClassTag]: Short = {

    typeOf[Scalar] match {
      case t if t =:= typeOf[Char]   => 2
      case t if t <:< typeOf[Short]  => 4
      case t if t <:< typeOf[Int]    => 8
      case t if t <:< typeOf[Float]  => 16
      case t if t <:< typeOf[Double] => 64
      case _                         => throw new Throwable(s"Unsupported datatype ${typeOf[Scalar]}")
    }
  }

  def writeVTK[D <: Dim: NDSpace: CanConvertToVTK, Scalar: Numeric: TypeTag: ClassTag](img: DiscreteScalarImage[D, Scalar], file: File): Try[Unit] = {

    val imgVtk = ImageConversion.imageTovtkStructuredPoints(img)
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

  def writeHDF5[D <: Dim, Scalar: TypeTag: ClassTag](img: DiscreteScalarImage[D, Scalar], file: File): Try[Unit] = {

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
      case t if t =:= typeOf[Float]  => Some("FLOAT")
      case t if t =:= typeOf[Short]  => Some("SHORT")
      case t if t =:= typeOf[Double] => Some("DOUBLE")
      case _                         => None
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

