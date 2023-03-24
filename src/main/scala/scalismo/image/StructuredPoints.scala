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
  Transformation,
  Translation2D,
  Translation3D,
  TranslationAfterRotation,
  TranslationAfterRotation2D,
  TranslationAfterRotation3D
}

import scala.language.implicitConversions

/**
 * Defines points in D dimension which are aligned on a regular grid.
 *
 * The grid points are defined by specifying an origin, a spacing between the grid points, and the size (number of
 * points) in each direction.
 *
 * A global coordinate system is assumed, and all units are measured in mm.
 *
 * @tparam D
 *   The dimensionality of the domain
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

  def continuousIndextoIndex(cidx: EuclideanVector[D]): IntVector[D] = {
    var d = 0
    val indexData = new Array[Int](dimensionality)
    while (d < dimensionality) {
      indexData(d) = Math.max(0, Math.min(Math.round(cidx(d)), size(d) - 1).toInt)
      d += 1
    }
    IntVector[D](indexData)
  }

  private def isIndex(continuousIndex: EuclideanVector[D]): Boolean =
    (0 until dimensionality).forall(i => continuousIndex(i) - Math.round(continuousIndex(i)) < 1e-8)

  def pointToContinuousIndex(pt: Point[D]): EuclideanVector[D]
  def pointToIndex(pt: Point[D]): IntVector[D] = continuousIndextoIndex(pointToContinuousIndex(pt))
  def indexToPoint(idx: IntVector[D]): Point[D]

  def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point[D]]]

  // define the canEqual method
  override def canEqual(a: Any) = a.isInstanceOf[StructuredPoints[D @unchecked]]

  override def equals(a: Any) = {
    a match {
      // make sure we can compare the 2 objects
      case c: StructuredPoints[D @unchecked] => {
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
case class StructuredPoints1D(origin: Point[_1D],
                              spacing: EuclideanVector[_1D],
                              size: IntVector[_1D],
                              iVec: EuclideanVector[_1D] = EuclideanVector1D(1.0)
) extends StructuredPoints[_1D] {

  require(Math.abs(iVec.norm - 1.0) < 1e-10)

  override def indexToPoint(idx: IntVector[_1D]): Point[_1D] = {
    val pointOnIVec = iVec * idx(0) * spacing(0)
    origin + pointOnIVec
  }

  override def pointToContinuousIndex(pt: Point[_1D]): EuclideanVector[_1D] = {
    val p0 = pt - origin
    EuclideanVector1D(p0.dot(iVec) * (1.0 / spacing(0)))
  }

  private def generateIterator(minX: Int, maxX: Int) = {
    for (i <- Iterator.range(minX, maxX)) yield { Point1D(origin.x + iVec.x * i * spacing.x) }
  }
  override def points: Iterator[Point1D] = generateIterator(0, size(0))

  // override def indexToPhysicalCoordinateTransform = transform

  override def index(linearIdx: PointId) = IntVector(linearIdx.id)
  override def pointId(idx: IntVector[_1D]) = PointId(idx(0))

  override val directions = SquareMatrix(iVec.toArray)

  //  private val transform = SimilarityTransformationSpace1D().transformForParameters(DenseVector(origin.data ++ spacing.data))
  //  private val inverseTransform = transform.inverse

  override def transform(t: Point[_1D] => Point[_1D]): UnstructuredPoints[_1D] = {
    new UnstructuredPoints1D(points.map(t).toIndexedSeq)
  }

  override def boundingBox: BoxDomain[_1D] = BoxDomain1D(origin, origin + EuclideanVector(size(0) * spacing(0)))

  override def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point1D]] = {
    require(nbChunks > 1)
    val chunkSize = size(0) / nbChunks
    val ranges = (0 until nbChunks).map { chunkId =>
      chunkId * chunkSize
    } :+ size(0)
    ranges.sliding(2).toIndexedSeq.map(minMaxX => generateIterator(minMaxX(0), minMaxX(1)))
  }

  override def toString: String = s"DiscreteImageDomain1D($size, $spacing, $boundingBox)"

}

object StructuredPoints1D {
  def apply(boundingBox: BoxDomain[_1D], size: IntVector[_1D]): StructuredPoints[_1D] = {
    val spacing = EuclideanVector1D(boundingBox.extent(0) / (size(0) + 1))
    StructuredPoints1D(boundingBox.origin, spacing, size)
  }

  def apply(boundingBox: BoxDomain[_1D], spacing: EuclideanVector[_1D]): StructuredPoints[_1D] = {
    val size = IntVector1D(Math.ceil(boundingBox.extent(0) / spacing(0)).toInt - 1)
    StructuredPoints1D(boundingBox.origin, spacing, size)
  }
}

case class StructuredPoints2D(origin: Point[_2D],
                              spacing: EuclideanVector[_2D],
                              size: IntVector[_2D],
                              iVec: EuclideanVector[_2D] = EuclideanVector2D(1.0, 0.0),
                              jVec: EuclideanVector[_2D] = EuclideanVector2D(0.0, 1.0)
) extends StructuredPoints[_2D] {

  require(Math.abs(iVec.norm - 1.0) < 1e-10)
  require(Math.abs(jVec.norm - 1.0) < 1e-10)

  private lazy val scaledDirectionMatrix: SquareMatrix[_2D] = directions * SquareMatrix.diag(
    EuclideanVector2D(spacing(0), spacing(1))
  )
  private lazy val inverseScaledDirectionMatrix: SquareMatrix[_2D] = SquareMatrix.inv(scaledDirectionMatrix)

  override val directions =
    SquareMatrix[_2D](iVec.toArray ++ jVec.toArray)

  override def indexToPoint(idx: IntVector[_2D]): Point[_2D] = {
    val indexVec = EuclideanVector2D(idx(0).toDouble, idx(1).toDouble)
    origin + scaledDirectionMatrix * indexVec
  }

  override def pointToContinuousIndex(pt: Point[_2D]): EuclideanVector[_2D] = {
    val p0 = pt - origin
    inverseScaledDirectionMatrix * p0
  }

  private def generateIterator(minY: Int, maxY: Int, minX: Int, maxX: Int) =
    for (j <- Iterator.range(minY, maxY); i <- Iterator.range(minX, maxX)) yield { indexToPoint(IntVector(i, j)) }

  override def points: Iterator[Point[_2D]] = generateIterator(0, size(1), 0, size(0))

  override def index(ptId: PointId) = (IntVector(ptId.id % size(0), ptId.id / size(0)))
  override def pointId(idx: IntVector[_2D]) = PointId(idx(0) + idx(1) * size(0))

  override def transform(t: Point[_2D] => Point[_2D]): UnstructuredPoints[_2D] = {
    new UnstructuredPoints2D(points.map(t).toIndexedSeq)
  }

  override def boundingBox: BoxDomain[_2D] = {
    val corners = Seq(
      IntVector(0, 0),
      IntVector(0, size(1) - 1),
      IntVector(size(0) - 1, 0),
      IntVector(size(0) - 1, size(1) - 1)
    ).map(indexToPoint)

    val minX = corners.map(_.x).min
    val minY = corners.map(_.y).min
    val maxX = corners.map(_.x).max
    val maxY = corners.map(_.y).max
    BoxDomain(Point2D(minX, minY), Point2D(maxX, maxY))
  }

  override def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point[_2D]]] = {
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
    new StructuredPoints2D(origin, spacing, size)
  }

  def apply(boundingBox: BoxDomain[_2D], size: IntVector[_2D]): StructuredPoints[_2D] = {
    val spacing = EuclideanVector2D(boundingBox.extent(0) / size(0), boundingBox.extent(1) / size(1))
    StructuredPoints2D(boundingBox.origin, spacing, size)
  }

  def apply(boundingBox: BoxDomain[_2D], spacing: EuclideanVector[_2D]): StructuredPoints[_2D] = {
    val size = IntVector2D(Math.ceil(boundingBox.extent(0) / spacing(0)).toInt,
                           Math.ceil(boundingBox.extent(1) / spacing(1)).toInt
    )

    StructuredPoints2D(boundingBox.origin, spacing, size)
  }

}

case class StructuredPoints3D(origin: Point[_3D],
                              spacing: EuclideanVector[_3D],
                              size: IntVector[_3D],
                              iVec: EuclideanVector[_3D] = EuclideanVector3D(1, 0, 0),
                              jVec: EuclideanVector[_3D] = EuclideanVector3D(0, 1, 0),
                              kVec: EuclideanVector[_3D] = EuclideanVector3D(0, 0, 1)
) extends StructuredPoints[_3D] {

  require(Math.abs(iVec.norm - 1.0) < 1e-10)
  require(Math.abs(jVec.norm - 1.0) < 1e-10)
  require(Math.abs(kVec.norm - 1.0) < 1e-10)

  override lazy val directions =
    SquareMatrix[_3D](iVec.toArray ++ jVec.toArray ++ kVec.toArray)

  private lazy val scaledDirectionMatrix: SquareMatrix[_3D] = directions * SquareMatrix.diag(
    EuclideanVector3D(spacing(0), spacing(1), spacing(2))
  )
  private lazy val inverseScaledDirectionMatrix: SquareMatrix[_3D] = SquareMatrix.inv(scaledDirectionMatrix)

  override def indexToPoint(idx: IntVector[_3D]): Point[_3D] = {
    val indexVec = EuclideanVector3D(idx(0).toDouble, idx(1).toDouble, idx(2).toDouble)
    origin + scaledDirectionMatrix * indexVec
  }

  override def pointToContinuousIndex(pt: Point[_3D]): EuclideanVector[_3D] = {
    val p0 = pt - origin
    val cidx = inverseScaledDirectionMatrix * p0
    cidx
  }

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
  private def generateIterator(minK: Int, maxK: Int, minY: Int, maxY: Int, minX: Int, maxX: Int) = {
    for (k <- Iterator.range(minK, maxK); j <- Iterator.range(minY, maxY); i <- Iterator.range(minX, maxX)) yield {
      indexToPoint(IntVector(i, j, k))
    }
  }
  override def points = generateIterator(0, size(2), 0, size(1), 0, size(0))

  override def pointsInChunks(nbChunks: Int): IndexedSeq[Iterator[Point[_3D]]] = {
    require(nbChunks > 1)
    val chunkSize = size(2) / nbChunks
    val ranges = (0 until nbChunks).map { chunkId =>
      chunkId * chunkSize
    } :+ size(2)
    ranges.sliding(2).toIndexedSeq.map(minMaxK => generateIterator(minMaxK(0), minMaxK(1), 0, size(1), 0, size(0)))
  }

  override def index(pointId: PointId) =
    IntVector(pointId.id % (size(0) * size(1)) % size(0),
              pointId.id % (size(0) * size(1)) / size(0),
              pointId.id / (size(0) * size(1))
    )

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
    new StructuredPoints3D(origin, spacing, size)
  }

  def apply(boundingBox: BoxDomain[_3D], size: IntVector[_3D]): StructuredPoints[_3D] = {
    val spacing = EuclideanVector3D(boundingBox.extent(0) / size(0),
                                    boundingBox.extent(1) / size(1),
                                    boundingBox.extent(2) / size(2)
    )
    StructuredPoints3D(boundingBox.origin, spacing, size)
  }

  def apply(boundingBox: BoxDomain[_3D], spacing: EuclideanVector[_3D]): StructuredPoints[_3D] = {
    val size = IntVector3D(
      Math.ceil(boundingBox.extent(0) / spacing(0)).toInt,
      Math.ceil(boundingBox.extent(1) / spacing(1)).toInt,
      Math.ceil(boundingBox.extent(2) / spacing(2)).toInt
    )

    StructuredPoints3D(boundingBox.origin, spacing, size)
  }

}

/**
 * Factory methods for creating StructuredPoints objects
 */
object StructuredPoints {

  /** Create a new discreteImageDomain with given origin, spacing and size */
  def apply[D](origin: Point[D], spacing: EuclideanVector[D], size: IntVector[D])(implicit
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
                        size: IntVector[_1D]
    ): StructuredPoints[_1D] = {
      StructuredPoints1D(origin, spacing, size)
    }

  }

  implicit object CreateStructuredPoints2D extends CreateStructuredPoints[_2D] {
    override def create(origin: Point[_2D],
                        spacing: EuclideanVector[_2D],
                        size: IntVector[_2D]
    ): StructuredPoints[_2D] = {
      new StructuredPoints2D(origin, spacing, size)
    }

  }

  implicit object CreateStructuredPoints3D extends CreateStructuredPoints[_3D] {
    override def create(origin: Point[_3D],
                        spacing: EuclideanVector[_3D],
                        size: IntVector[_3D]
    ): StructuredPoints[_3D] = {
      new StructuredPoints3D(origin, spacing, size)
    }
  }
}
