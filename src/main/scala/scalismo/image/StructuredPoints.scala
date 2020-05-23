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
package scalismo.image

import breeze.linalg.{diag, DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.geometry._
import scalismo.transformations.{
  Rotation,
  Rotation2D,
  Rotation3D,
  RotationThenTranslation,
  RotationThenTranslation2D,
  RotationThenTranslation3D,
  Transformation,
  Translation,
  Translation2D,
  Translation3D
}

import scala.language.implicitConversions

/**
 * Defines points in D dimension which are aligned on a regular grid.
 *
 * The grid points are defined by specifying an origin, a spacing between the grid points,
 * and the size (number of points) in each direction.
 *
 * A global coordinate system is assumed, and all units are measured in mm.
 *
 * @tparam D The dimensionality of the domain
 */
abstract class StructuredPoints[D: NDSpace] extends PointSet[D] with Equals {

  /** the first point (lower-left corner in 2D) of the grid */
  def origin: Point[D]

  /** the distance (in mm) between two points in each space direction */
  def spacing: EuclideanVector[D]

  /** the number of points in each direction */
  def size: IntVector[D]

  /** Direction cosine matrix */
  def directions: SquareMatrix[D]

  /** the dimensionality of the domain */
  val dimensionality = implicitly[NDSpace[D]].dimensionality

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  /**
   * returns the points belonging to the given id
   */
  override def point(id: PointId): Point[D] = indexToPoint(index(id))

  /** converts a grid index into a id that identifies a point */
  def pointId(idx: IntVector[D]): PointId

  /** The index for the given point id */
  def index(pointId: PointId): IntVector[D]

  /**
   * a rectangular region that represents the area, which defines the bounding box of the points
   */
  override def boundingBox: BoxDomain[D]

  /** true if the point is part of the grid points */
  override def isDefinedAt(pt: Point[D]): Boolean = {
    boundingBox.isDefinedAt(pt) && isIndex(pointToContinuousIndex(pt))
  }

  /** true if the point is part of the grid points */
  def isDefinedAt(idx: IntVector[D]): Boolean = {
    (0 until dimensionality).forall(i => idx(i) < size(i))
  }

  /** returns the point id in case it is defined, None otherwise. */
  override def pointId(pt: Point[D]): Option[PointId] = {
    val cidx = pointToContinuousIndex(pt)
    val ptId = pointId(continuousIndextoIndex(cidx))
    if (isIndex(cidx)) Some(ptId) else None
  }

  /**
   * returns the closest point in this set to the given point
   */
  override def findClosestPoint(pt: Point[D]): PointWithId[D] = {
    val cidx = pointToContinuousIndex(pt)
    val idxClosestPoint = continuousIndextoIndex(cidx)
    val ptIdClosestPoint = pointId(idxClosestPoint)
    PointWithId(indexToPoint(idxClosestPoint), ptIdClosestPoint)
  }

  /**
   * returns the n closest points to the given set of points
   */
  override def findNClosestPoints(pt: Point[D], n: Int): Seq[PointWithId[D]] = throw new UnsupportedOperationException

  private def continuousIndextoIndex(cidx: EuclideanVector[D]): IntVector[D] = {
    var d = 0
    val indexData = new Array[Int](dimensionality)
    while (d < dimensionality) {
      indexData(d) = Math.min(Math.round(cidx(d)), size(d) - 1).toInt
      d += 1
    }
    IntVector[D](indexData)
  }

  private def pointToContinuousIndex(pt: Point[D]): EuclideanVector[D] = {
    physicalCoordinateToContinuousIndex(pt).toVector
  }

  def indexToPoint(i: IntVector[D]) = indexToPhysicalCoordinateTransform(Point[D](i.toArray.map(_.toDouble)))

  private def isIndex(continuousIndex: EuclideanVector[D]): Boolean =
    (0 until dimensionality).forall(i => continuousIndex(i) - Math.round(continuousIndex(i)) < 1e-8)

  /** the anisotropic similarity transform that maps between the index and physical coordinates*/
  private[scalismo] def indexToPhysicalCoordinateTransform: Transformation[D]
  private[scalismo] def physicalCoordinateToContinuousIndex: Transformation[D]

  /**
   * *
   * Returns a sequence of iterators on the domain points, the size of the sequence being indicated by the user.
   *
   * The main idea behind this method is to be able to easily parallelize on the domain points, as parallel operations
   * on a single iterator in Scala end up more costly than sequential access in our case. Using this method, one would parallelize on the
   * IndexedSeq of iterators instead.
   *
   */
  private[scalismo] def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point[D]]]

  // define the canEqual method
  override def canEqual(a: Any) = a.isInstanceOf[StructuredPoints[D]]

  override def equals(a: Any) = {
    a match {
      // make sure we can compare the 2 objects
      case c: StructuredPoints[D] => {
        c.canEqual(this) &&
        origin == c.origin &&
        spacing == c.spacing &&
        size == c.size &&
        directions == c.directions
      }
      case other => false
    }
  }

  override def hashCode() = origin.hashCode + spacing.hashCode + size.hashCode
}

//
// The actual implementations for each dimension
//
case class StructuredPoints1D(origin: Point[_1D], spacing: EuclideanVector[_1D], size: IntVector[_1D])
    extends StructuredPoints[_1D] {

  override val indexToPhysicalCoordinateTransform: Transformation[_1D] = new Transformation[_1D] {
    override def domain: Domain[_1D] = RealSpace[_1D]
    override def f: Point[_1D] => Point[_1D] = pt => {
      val scaledPoint = Point1D(pt(0) * spacing(0))
      scaledPoint + origin.toVector
    }
  }

  override private[scalismo] val physicalCoordinateToContinuousIndex: Transformation[_1D] =
    new Transformation[_1D] {
      override def domain: Domain[_1D] = RealSpace[_1D]
      override def f: Point[_1D] => Point[_1D] = pt => {
        val pointInStandardPosition = pt - origin
        Point1D(pointInStandardPosition(0) / spacing(0))
      }
    }

  private val iVecImage
    : EuclideanVector1D = indexToPhysicalCoordinateTransform(Point(1)) - indexToPhysicalCoordinateTransform(Point(0))

  private def generateIterator(minX: Int, maxX: Int) = {
    for (i <- Iterator.range(minX, maxX)) yield { Point1D(origin.x + iVecImage.x * i) }
  }
  override def points: Iterator[Point1D] = generateIterator(0, size(0))

  //override def indexToPhysicalCoordinateTransform = transform

  override def index(linearIdx: PointId) = IntVector(linearIdx.id)
  override def pointId(idx: IntVector[_1D]) = PointId(idx(0))

  override val directions = SquareMatrix(1.0)

  //  private val transform = SimilarityTransformationSpace1D().transformForParameters(DenseVector(origin.data ++ spacing.data))
  //  private val inverseTransform = transform.inverse

  override def transform(t: Point[_1D] => Point[_1D]): UnstructuredPoints[_1D] = {
    new UnstructuredPoints1D(points.map(t).toIndexedSeq)
  }

  override def boundingBox: BoxDomain[_1D] = BoxDomain1D(origin, origin + EuclideanVector(size(0) * spacing(0)))

  override private[scalismo] def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point1D]] = {
    require(nbChunks > 1)
    val chunkSize = size(0) / nbChunks
    val ranges = (0 until nbChunks).map { chunkId =>
      chunkId * chunkSize
    } :+ size(0)
    ranges.sliding(2).toIndexedSeq.map(minMaxX => generateIterator(minMaxX(0), minMaxX(1)))
  }

  override def toString: String = s"DiscreteImageDomain1D($size, $spacing, $boundingBox)"

}

case class StructuredPoints2D(origin: Point[_2D], spacing: EuclideanVector[_2D], size: IntVector[_2D], phi: Double)
    extends StructuredPoints[_2D] {

  private val rigidTransform =
    RotationThenTranslation2D(Rotation2D(phi, Point2D(0, 0)), Translation2D(origin - Point2D(0.0, 0.0)))
  private val invRigidTransform = rigidTransform.inverse

  override val indexToPhysicalCoordinateTransform: Transformation[_2D] = new Transformation[_2D] {
    override def domain: Domain[_2D] = EuclideanSpace[_2D]
    override def f: Point[_2D] => Point[_2D] = pt => {
      val scaledPoint = Point2D(pt(0) * spacing(0), pt(1) * spacing(1))
      rigidTransform(scaledPoint)
    }
  }

  override private[scalismo] val physicalCoordinateToContinuousIndex: Transformation[_2D] =
    new Transformation[_2D] {
      override def domain: Domain[_2D] = EuclideanSpace[_2D]
      override def f: Point[_2D] => Point[_2D] = pt => {
        val pointInStandardPosition = invRigidTransform(pt)
        Point2D(pointInStandardPosition(0) / spacing(0), pointInStandardPosition(1) / spacing(1))
      }
    }

  private val iVecImage
    : EuclideanVector2D = indexToPhysicalCoordinateTransform(Point(1, 0)) - indexToPhysicalCoordinateTransform(
    Point(0, 0)
  )
  private val jVecImage
    : EuclideanVector2D = indexToPhysicalCoordinateTransform(Point(0, 1)) - indexToPhysicalCoordinateTransform(
    Point(0, 0)
  )

  override val directions =
    SquareMatrix[_2D]((iVecImage * (1.0 / iVecImage.norm)).toArray ++ (jVecImage * (1.0 / jVecImage.norm)).toArray)

  private def generateIterator(minY: Int, maxY: Int, minX: Int, maxX: Int) =
    for (j <- Iterator.range(minY, maxY); i <- Iterator.range(minX, maxX)) yield { ijToPoint(i, j) }

  override def points: Iterator[Point2D] = generateIterator(0, size(1), 0, size(0))

  override def index(ptId: PointId) = (IntVector(ptId.id % size(0), ptId.id / size(0)))
  override def pointId(idx: IntVector[_2D]) = PointId(idx(0) + idx(1) * size(0))

  override def transform(t: Point[_2D] => Point[_2D]): UnstructuredPoints[_2D] = {
    new UnstructuredPoints2D(points.map(t).toIndexedSeq)
  }

  @inline private def ijToPoint(i: Int, j: Int) =
    Point2D(origin.x + iVecImage.x * i + jVecImage.x * j, origin.y + iVecImage.y * i + jVecImage.y * j)

  override def indexToPoint(i: IntVector[_2D]) = {
    val idx: IntVector2D = i
    ijToPoint(idx.i, idx.j)
  }

  override def boundingBox: BoxDomain[_2D] = {
    val extendData = (0 until 2).map(i => size(i) * spacing(i))
    val extent = EuclideanVector[_2D](extendData.toArray)
    val oppositeCorner = origin + extent
    BoxDomain2D(origin, oppositeCorner)
  }

  override private[scalismo] def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point2D]] = {
    require(nbChunks > 1)
    val chunkSize = size(1) / nbChunks
    val ranges = (0 until nbChunks).map { chunkId =>
      chunkId * chunkSize
    } :+ size(1)
    ranges.sliding(2).toIndexedSeq.map(minMaxY => generateIterator(minMaxY(0), minMaxY(1), 0, size(0)))
  }

  override def toString: String = s"DiscreteImageDomain2D($size, $spacing, $boundingBox)"

}

object StructuredPoints2D {

  def apply(origin: Point[_2D], spacing: EuclideanVector[_2D], size: IntVector[_2D]): StructuredPoints[_2D] = {
    StructuredPoints2D(origin, spacing, size, phi = 0.0)
  }

}

case class StructuredPoints3D(origin: Point[_3D],
                              spacing: EuclideanVector[_3D],
                              size: IntVector[_3D],
                              phi: Double,
                              theta: Double,
                              psi: Double)
    extends StructuredPoints[_3D] {

  val rigidTransform = RotationThenTranslation3D(Rotation3D(phi, theta, psi, Point3D(0, 0, 0)),
                                                 Translation3D(origin - Point3D(0.0, 0.0, 0.0)))
  private val invRigidTransform = rigidTransform.inverse

  override private[scalismo] val indexToPhysicalCoordinateTransform: Transformation[_3D] = new Transformation[_3D] {
    override def domain: Domain[_3D] = EuclideanSpace3D
    override def f: Point[_3D] => Point[_3D] = pt => {
      val scaledPoint = Point3D(pt(0) * spacing(0), pt(1) * spacing(1), pt(2) * spacing(2))
      rigidTransform(scaledPoint)
    }
  }

  override private[scalismo] val physicalCoordinateToContinuousIndex: Transformation[_3D] =
    new Transformation[_3D] {
      override def domain: Domain[_3D] = EuclideanSpace3D
      override def f: Point[_3D] => Point[_3D] = pt => {
        val pointInStandardPosition = invRigidTransform(pt)
        Point3D(pointInStandardPosition(0) / spacing(0),
                pointInStandardPosition(1) / spacing(1),
                pointInStandardPosition(2) / spacing(2))
      }
    }

  private val positiveScalingParameters = spacing.map(math.abs)

  override def boundingBox: BoxDomain[_3D] = {

    val corners = List(
      IntVector(0, 0, 0),
      IntVector(size(0) - 1, 0, 0),
      IntVector(0, size(1) - 1, 0),
      IntVector(0, 0, size(2) - 1),
      IntVector(size(0) - 1, size(1) - 1, 0),
      IntVector(size(0) - 1, 0, size(2) - 1),
      IntVector(0, size(1) - 1, size(2) - 1),
      IntVector(size(0) - 1, size(1) - 1, size(2) - 1)
    )
    val cornerImages = corners.map(i => indexToPoint(i))

    val originX = cornerImages.map(p => p(0)).min
    val originY = cornerImages.map(p => p(1)).min
    val originZ = cornerImages.map(p => p(2)).min

    val oppositeX = cornerImages.map(p => p(0)).max
    val oppositeY = cornerImages.map(p => p(1)).max
    val oppositeZ = cornerImages.map(p => p(2)).max

    BoxDomain3D(Point(originX, originY, originZ), Point(oppositeX, oppositeY, oppositeZ))
  }

  private val iVecImage
    : EuclideanVector3D = indexToPhysicalCoordinateTransform(Point(1, 0, 0)) - indexToPhysicalCoordinateTransform(
    Point(0, 0, 0)
  )
  private val jVecImage
    : EuclideanVector3D = indexToPhysicalCoordinateTransform(Point(0, 1, 0)) - indexToPhysicalCoordinateTransform(
    Point(0, 0, 0)
  )
  private val kVecImage
    : EuclideanVector3D = indexToPhysicalCoordinateTransform(Point(0, 0, 1)) - indexToPhysicalCoordinateTransform(
    Point(0, 0, 0)
  )

  val directions = SquareMatrix[_3D](
    ((iVecImage * (1.0 / iVecImage.norm)).toArray
      ++ (jVecImage * (1.0 / jVecImage.norm)).toArray
      ++ (kVecImage * (1.0 / kVecImage.norm)).toArray)
  )

  private def generateIterator(minK: Int, maxK: Int, minY: Int, maxY: Int, minX: Int, maxX: Int) = {
    for (k <- Iterator.range(minK, maxK); j <- Iterator.range(minY, maxY); i <- Iterator.range(minX, maxX)) yield {
      ijkToPoint(i, j, k)
    }
  }
  override def points = generateIterator(0, size(2), 0, size(1), 0, size(0))

  override private[scalismo] def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point3D]] = {
    require(nbChunks > 1)
    val chunkSize = size(2) / nbChunks
    val ranges = (0 until nbChunks).map { chunkId =>
      chunkId * chunkSize
    } :+ size(2)
    ranges.sliding(2).toIndexedSeq.map(minMaxK => generateIterator(minMaxK(0), minMaxK(1), 0, size(1), 0, size(0)))
  }

  @inline private def ijkToPoint(i: Int, j: Int, k: Int) = {
    Point3D(
      origin.x + iVecImage.x * i + jVecImage.x * j + kVecImage.x * k,
      origin.y + iVecImage.y * i + jVecImage.y * j + kVecImage.y * k,
      origin.z + iVecImage.z * i + jVecImage.z * j + kVecImage.z * k
    )
  }

  override def indexToPoint(indx: IntVector[_3D]) = {
    val idx: IntVector3D = indx
    ijkToPoint(idx.i, idx.j, idx.k)
  }

  override def index(pointId: PointId) =
    IntVector(pointId.id % (size(0) * size(1)) % size(0),
              pointId.id % (size(0) * size(1)) / size(0),
              pointId.id / (size(0) * size(1)))

  override def pointId(idx: IntVector[_3D]): PointId = {
    PointId(idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1))
  }

  override def transform(t: Point[_3D] => Point[_3D]): UnstructuredPoints[_3D] = {
    new UnstructuredPoints3D(points.map(t).toIndexedSeq)
  }

  override def toString: String = s"DiscreteImageDomain3D($size, $spacing, $boundingBox)"

}

object StructuredPoints3D {

  def apply(origin: Point[_3D], spacing: EuclideanVector[_3D], size: IntVector[_3D]): StructuredPoints[_3D] = {
    StructuredPoints3D(origin, spacing, size, 0, 0, 0)
  }
}

/**
 * Factory methods for creating StructuredPoints objects
 */
object StructuredPoints {

  /** Create a new discreteImageDomain with given origin, spacing and size*/
  def apply[D](origin: Point[D], spacing: EuclideanVector[D], size: IntVector[D])(
    implicit
    evCreate: CreateStructuredPoints[D]
  ) = {
    evCreate.create(origin, spacing, size)
  }

  implicit def parametricToConcreteType1D(discreteImageDomain: StructuredPoints[_1D]): StructuredPoints1D = {
    discreteImageDomain.asInstanceOf[StructuredPoints1D]
  }

  implicit def parametricToConcreteType2D(discreteImageDomain: StructuredPoints[_2D]): StructuredPoints2D = {
    discreteImageDomain.asInstanceOf[StructuredPoints2D]
  }

  implicit def paramDiscreteImageDomain3D(discreteImageDomain: StructuredPoints[_3D]): StructuredPoints3D = {
    discreteImageDomain.asInstanceOf[StructuredPoints3D]
  }

}

/** Typeclass for creating domains of arbitrary dimensionality */
sealed trait CreateStructuredPoints[D] {
  def create(origin: Point[D], spacing: EuclideanVector[D], size: IntVector[D]): StructuredPoints[D]
}

object CreateStructuredPoints {
  implicit object CreateStructuredPoints1D extends CreateStructuredPoints[_1D] {
    override def create(origin: Point[_1D],
                        spacing: EuclideanVector[_1D],
                        size: IntVector[_1D]): StructuredPoints[_1D] = {
      StructuredPoints1D(origin, spacing, size)
    }

  }

  implicit object CreateStructuredPoints2D extends CreateStructuredPoints[_2D] {
    override def create(origin: Point[_2D],
                        spacing: EuclideanVector[_2D],
                        size: IntVector[_2D]): StructuredPoints[_2D] = {
      new StructuredPoints2D(origin, spacing, size, 0.0)
    }

  }

  implicit object CreateStructuredPoints3D extends CreateStructuredPoints[_3D] {
    override def create(origin: Point[_3D],
                        spacing: EuclideanVector[_3D],
                        size: IntVector[_3D]): StructuredPoints[_3D] = {
      new StructuredPoints3D(origin, spacing, size, 0.0, 0.0, 0.0)
    }
  }
}
