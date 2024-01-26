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

import breeze.linalg.{diag, inv, DenseMatrix, DenseVector}
import niftijio.NiftiVolume
import scalismo.common.Scalar
import scalismo.geometry.{_3D, EuclideanVector, EuclideanVector3D, IntVector, IntVector3D, Point}
import scalismo.image.{DiscreteImage, DiscreteImageDomain, StructuredPoints, StructuredPoints3D}

import java.io.File
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Implements methods for reading and writing D-dimensional images
 *
 * '''WARNING! WE ARE USING an LPS WORLD COORDINATE SYSTEM'''
 *
 * VTK file format does not indicate the orientation of the image. Therefore, when reading from VTK, we assume that it
 * is in RAI orientation. Hence, no magic is done, the same information (coordinates) present in the VTK file header are
 * directly mapped to our coordinate system.
 *
 * This is also the case when writing VTK. Our image domain information (origin, spacing ..) is mapped directly into the
 * written VTK file header.
 *
 * This is however not the case for Nifti files! Nifti file headers contain an affine transform from the ijk image
 * coordinates to an RAS World Coordinate System (therefore supporting different image orientations). In order to read
 * Nifti files coherently, we need to adapt the obtained RAS coordinates to our LPS system :
 *
 * This is done by mirroring the first two dimensions of each point after applying the affine transform
 *
 * The same mirroring is done again when writing an image to the Nifti format.
 *
 * '''Note on Nifti's qform and sform :'''
 *
 * As mentioned above, the Nifti header contains a transform from the unit ijk grid to the RAS world coordinates of the
 * grid. This transform can be encoded in 2 entries of the Nifti header, the qform and the sform. In some files, these 2
 * entries can both be present, and in some cases could even indicate different transforms. In Scalismo, when such a
 * case happens, we favour the sform entry by default. If you wish instead to favour the qform transform, you can do so
 * by setting a flag appropriately in the [[scalismo.io.ImageIO.read3DScalarImage]] method.
 *
 * ''' Documentation on orientation :'''
 *
 * http://www.grahamwideman.com/gw/brain/orientation/orientterms.htm
 *
 * http://www.slicer.org/slicerWiki/index.php/Coordinate_systems
 *
 * http://brainder.org/2012/09/23/the-nifti-file-format/
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

  private lazy val RAStoLPSMatrix = DenseMatrix((-1.0, 0.0, 0.0), (0.0, -1.0, 0.0), (0.0, 0.0, 1.0))
  private lazy val LPStoRASMatrix = inv(RAStoLPSMatrix)

  def readNifti[S: Scalar: ClassTag](file: File, favourQform: Boolean = false): Try[DiscreteImage[_3D, S]] = {

    for {
      volume <- FastReadOnlyNiftiVolume.read(file.getAbsolutePath)
      voxelToLPSCoordinateTransform <- computeVoxelToLPSCoordinateTransform(volume, favourQform)
      image <- createImageWithRightOrientation(volume, voxelToLPSCoordinateTransform)
    } yield {

      image
    }
  }

  private def createImageWithRightOrientation[S: Scalar: ClassTag](
    volume: FastReadOnlyNiftiVolume,
    voxelToLPSCoordinateTransform: IntVector[_3D] => Point[_3D]
  ): Try[DiscreteImage[_3D, S]] = {

    val expectedScalarType = ScalarDataType.fromType[S]

    // If a volume has a transform, we always treat it as a float image and convert accordingly.
    // Otherwise we take the type that is found in the nifty header
    val foundScalarType =
      if (volume.hasTransform) ScalarDataType.fromType[Float]
      else ScalarDataType.fromNiftiId(volume.header.datatype)

    if (expectedScalarType != foundScalarType) {
      Failure(
        new IllegalArgumentException(
          s"Invalid scalar type (expected $expectedScalarType, found $foundScalarType)"
        )
      )
    } else {

      // First we compute origin, spacing and size
      val origin = voxelToLPSCoordinateTransform(IntVector3D(0, 0, 0))
      val nx = volume.header.dim(1)
      val ny = volume.header.dim(2)
      val nz = volume.header.dim(3)
      val dim = if (volume.header.dim(4) == 0) 1 else volume.header.dim(4)

      val spacing = EuclideanVector3D(volume.header.pixdim(1), volume.header.pixdim(2), volume.header.pixdim(3))
      val size = IntVector(nx, ny, nz)

      def toUnitVec(v: EuclideanVector[_3D]): EuclideanVector[_3D] = {
        v * (1.0 / v.norm)
      }

      // By transforming the standard coordinate vectors to LPS space we can figure
      // out what the direction matrix should be
      val iVec = toUnitVec(voxelToLPSCoordinateTransform(IntVector3D(1, 0, 0)) - origin)
      val jVec = toUnitVec(voxelToLPSCoordinateTransform(IntVector3D(0, 1, 0)) - origin)
      val kVec = toUnitVec(voxelToLPSCoordinateTransform(IntVector3D(0, 0, 1)) - origin)

      // now we assemble everything
      val newDomain = DiscreteImageDomain(
        StructuredPoints3D(origin, EuclideanVector(spacing(0), spacing(1), spacing(2)), size, iVec, jVec, kVec)
      )
      val im = DiscreteImage(newDomain, volume.dataAsScalarArray)

      Success(im)
    }
  }

  /* Returns the augmented matrix of the affine transform from ijk to xyz_RAS
   * The logic is based on: http://brainder.org/2012/09/23/the-nifti-file-format/
   * (section "Orientation information").
   */
  private[this] def transformMatrixFromNifti(volume: FastReadOnlyNiftiVolume,
                                             favourQform: Boolean
  ): Try[DenseMatrix[Double]] = {
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
        // Attention: we're by default ignoring the q value here, and solely basing the decision on s != 0, unless the user says so
        if (favourQform)
          Success(DenseMatrix.create(4, 4, volume.header.qform_to_mat44.flatten).t)
        else
          Success(DenseMatrix.create(4, 4, volume.header.sformArray).t)
    }
  }

  /**
   * constructs a transformation that takes an index and transforms it to LPS coordinates
   */
  private[this] def computeVoxelToLPSCoordinateTransform(
    volume: FastReadOnlyNiftiVolume,
    favourQform: Boolean
  ): Try[IntVector[_3D] => Point[_3D]] = {
    var dim = volume.header.dim(4)

    if (dim == 0)
      dim = 1

    // check this page http://brainder.org/2012/09/23/the-nifti-file-format/
    // for details about the nifti format

    transformMatrixFromNifti(volume, favourQform).map { affineTransMatrix =>
      {
        // the affine matrix is in homogeneous coordinates. We extract the individual components
        // and apply them individually
        val rotationAndScalingMatrix: DenseMatrix[Double] = affineTransMatrix(0 to 2, 0 to 2)
        val translationVector = DenseVector(affineTransMatrix(0, 3), affineTransMatrix(1, 3), affineTransMatrix(2, 3))
        (x: IntVector[_3D]) => {

          val pointInRas =
            (rotationAndScalingMatrix * DenseVector(x(0).toDouble, x(1).toDouble, x(2).toDouble) + translationVector)
          val pointInLPS = RAStoLPSMatrix * pointInRas
          Point(pointInLPS(0), pointInLPS(1), pointInLPS(2))
        }
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
        LPStoRASMatrix * domain.directions.toBreezeMatrix * scalingMatrix
      }

      val originInRAS = LPStoRASMatrix * domain.pointSet.origin.toBreezeVector
      val innerAffineMatrix = computeInnerAffineMatrix(img.domain.pointSet)
      val M = DenseMatrix.zeros[Double](4, 4)

      M(0, 0) = innerAffineMatrix(0, 0)
      M(0, 1) = innerAffineMatrix(0, 1)
      M(0, 2) = innerAffineMatrix(0, 2)
      M(0, 3) = originInRAS(0)
      M(1, 0) = innerAffineMatrix(1, 0)
      M(1, 1) = innerAffineMatrix(1, 1)
      M(1, 2) = innerAffineMatrix(1, 2)
      M(1, 3) = originInRAS(1)
      M(2, 0) = innerAffineMatrix(2, 0)
      M(2, 1) = innerAffineMatrix(2, 1)
      M(2, 2) = innerAffineMatrix(2, 2)
      M(2, 3) = originInRAS(2)
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

}
