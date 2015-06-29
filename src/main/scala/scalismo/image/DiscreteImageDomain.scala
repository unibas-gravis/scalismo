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

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry._
import scalismo.registration.{ TranslationTransform, AnisotropicSimilarityTransformationSpace, SimilarityTransformationSpace1D, AnisotropicSimilarityTransformation }
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
abstract class DiscreteImageDomain[D <: Dim: NDSpace] extends DiscreteDomain[D] with Equals {

  /** the first point (lower-left corner in 2D) of the grid */
  def origin: Point[D]

  /** the distance (in mm) between two points in each space direction */
  def spacing: Vector[D]

  /** the number of points in each direction */
  def size: Index[D]

  /** Direction cosine matrix */
  def directions: SquareMatrix[D]

  /** the dimensionality of the domain */
  val dimensionality = implicitly[NDSpace[D]].dimensionality

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  override def point(id: Int): Point[D] = indexToPoint(index(id))

  /** converts a grid index into a id that identifies a point */
  def pointId(idx: Index[D]): Int

  /** The index for the given point id */
  def index(pointId: Int): Index[D]

  /** the point corresponding to the given index */
  //def indexToPoint(i: Index[D]): Point[D]

  /** the index correspoinding to the physical point coordinate */
  //def pointToIndex(p: Point[D]): Index[D]

  /**
   * a rectangular region that represents the area over which an image is defined by the points
   * that represent this image.
   */
  override def boundingBox: BoxDomain[D] = {
    val extendData = (0 until dimensionality).map(i => size(i) * spacing(i))
    val extent = Vector[D](extendData.toArray)
    val oppositeCorner = origin + extent
    BoxDomain(origin, oppositeCorner)
  }

  /** true if the point is part of the grid points */
  override def isDefinedAt(pt: Point[D]): Boolean = {
    isIndex(pointToContinuousIndex(pt))
  }

  /** returns the point id in case it is defined, None otherwise. */
  override def pointId(pt: Point[D]): Option[Int] = {
    val cidx = pointToContinuousIndex(pt)
    val ptId = pointId(continuousIndextoIndex(cidx))
    if (isIndex(cidx)) Some(ptId) else None
  }

  override def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
    val cidx = pointToContinuousIndex(pt)
    val idxClosestPoint = continuousIndextoIndex(cidx)
    val ptIdClosestPoint = pointId(idxClosestPoint)
    (indexToPoint(idxClosestPoint), ptIdClosestPoint)
  }

  override def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] = ???

  private def continuousIndextoIndex(cidx: Vector[D]): Index[D] = {
    var d = 0;
    val indexData = new Array[Int](dimensionality)
    while (d < dimensionality) {
      indexData(d) = Math.min(Math.round(cidx(d)), size(d) - 1)
      d += 1
    }
    Index[D](indexData)
  }

  private def pointToContinuousIndex(pt: Point[D]): Vector[D] = {
    val data = (0 until dimensionality).map(i => (pt(i) - origin(i)) / spacing(i))
    Vector[D](data.toArray)
  }

  def indexToPoint(i: Index[D]) = {
    indexToPhysicalCoordinateTransform(Point[D](i.data.map(_.toFloat)))
  }

  private def isIndex(continousIndex: Vector[D]): Boolean = {
    (0 until dimensionality).forall(i => (continousIndex(i) - Math.round(continousIndex(i)) < 1e-8))
  }

  /** the anisotropic similarity transform that maps between the index and physical coordinates*/
  private[scalismo] def indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[D]

  // define the canEqual method
  override def canEqual(a: Any) = a.isInstanceOf[DiscreteImageDomain[D]]

  override def equals(a: Any) = {
    a match {
      // make sure we can compare the 2 objects
      case c: DiscreteImageDomain[D] => {
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

/**
 * Factory methods for creating DiscreteImageDomain objects
 */
object DiscreteImageDomain {

  /** Create a new discreteImageDomain with given origin, spacing and size*/
  def apply[D <: Dim](origin: Point[D], spacing: Vector[D], size: Index[D])(implicit evDim: NDSpace[D], evCreate: CreateDiscreteImageDomain[D]) = {
    evCreate.createImageDomain(origin, spacing, size)
  }

  /** Create a new discreteImageDomain with given image box (i.e. a box that determines the area where the image is defined) and size */
  def apply[D <: Dim](imageBox: BoxDomain[D], size: Index[D])(implicit evDim: NDSpace[D], evCreate: CreateDiscreteImageDomain[D]): DiscreteImageDomain[D] = {
    val spacing = imageBox.extent.mapWithIndex({ case (ithExtent, i) => ithExtent / size(i) })
    evCreate.createImageDomain(imageBox.origin, spacing, size)
  }

  /** Create a new discreteImageDomain with given image box (i.e. a box that determines the area where the image is defined) and size */
  def apply[D <: Dim](imageBox: BoxDomain[D], spacing: Vector[D])(implicit evDim: NDSpace[D], evCreate: CreateDiscreteImageDomain[D]): DiscreteImageDomain[D] = {
    val sizeFractional = imageBox.extent.mapWithIndex({ case (ithExtent, i) => ithExtent / spacing(i) })
    val size = Index.apply[D](sizeFractional.data.map(s => Math.ceil(s).toInt))
    evCreate.createImageDomain(imageBox.origin, spacing, size)
  }

  /**
   * Create a discreteImageDomain where the points are defined as tranformations of the indeces (from (0,0,0) to (size - 1, size - 1 , size -1)
   * This makes it possible to define image regions which are not aligned to the coordinate axis.
   */
  private[scalismo] def apply[D <: Dim](size: Index[D], transform: AnisotropicSimilarityTransformation[D])(implicit evDim: NDSpace[D], evCreateRot: CreateDiscreteImageDomain[D]) = {
    evCreateRot.createWithTransform(size, transform)
  }

  implicit def parametricToConcreteType1D(discreteImageDomain: DiscreteImageDomain[_1D]): DiscreteImageDomain1D = {
    discreteImageDomain.asInstanceOf[DiscreteImageDomain1D]
  }

  implicit def parametricToConcreteType2D(discreteImageDomain: DiscreteImageDomain[_2D]): DiscreteImageDomain2D = {
    discreteImageDomain.asInstanceOf[DiscreteImageDomain2D]
  }

  implicit def paramDiscreteImageDomain3D(discreteImageDomain: DiscreteImageDomain[_3D]): DiscreteImageDomain3D = {
    discreteImageDomain.asInstanceOf[DiscreteImageDomain3D]
  }

}

//
// The actual implementations for each dimension
//
case class DiscreteImageDomain1D(size: Index[_1D], indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[_1D]) extends DiscreteImageDomain[_1D] {

  def origin = indexToPhysicalCoordinateTransform(Point(0))
  private val iVecImage = indexToPhysicalCoordinateTransform(Point(1)) - indexToPhysicalCoordinateTransform(Point(0))
  override def spacing = Vector(iVecImage.norm.toFloat)
  def points = for (i <- (0 until size(0)).toIterator) yield Point(origin(0) + spacing(0) * i) // TODO replace with operator version

  //override def indexToPhysicalCoordinateTransform = transform

  override def index(linearIdx: Int) = Index(linearIdx)
  override def pointId(idx: Index[_1D]) = idx(0)

  override val directions = SquareMatrix(1.0f)

  //  private val transform = SimilarityTransformationSpace1D().transformForParameters(DenseVector(origin.data ++ spacing.data))
  //  private val inverseTransform = transform.inverse

  override def transform(t: Point[_1D] => Point[_1D]): UnstructuredPointsDomain[_1D] = {
    new UnstructuredPointsDomain1D(points.map(t).toIndexedSeq)
  }

}

case class DiscreteImageDomain2D(size: Index[_2D], indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[_2D]) extends DiscreteImageDomain[_2D] {

  private val inverseAnisotropicTransform = indexToPhysicalCoordinateTransform.inverse

  def origin = indexToPhysicalCoordinateTransform(Point(0, 0))

  private val iVecImage = indexToPhysicalCoordinateTransform(Point(1, 0)) - indexToPhysicalCoordinateTransform(Point(0, 0))
  private val jVecImage = indexToPhysicalCoordinateTransform(Point(0, 1)) - indexToPhysicalCoordinateTransform(Point(0, 0))

  override val directions = SquareMatrix[_2D]((iVecImage * (1.0 / iVecImage.norm)).data ++ (jVecImage * (1.0 / jVecImage.norm)).data)
  override def spacing = Vector(iVecImage.norm.toFloat, jVecImage.norm.toFloat)

  def points = for (j <- (0 until size(1)).toIterator; i <- (0 until size(0)).view) yield indexToPhysicalCoordinateTransform(Point(i, j))

  override def index(ptId: Int) = (Index(ptId % size(0), ptId / size(0)))
  override def pointId(idx: Index[_2D]) = idx(0) + idx(1) * size(0)

  override def transform(t: Point[_2D] => Point[_2D]): UnstructuredPointsDomain[_2D] = {
    new UnstructuredPointsDomain2D(points.map(t).toIndexedSeq)
  }

}

case class DiscreteImageDomain3D(size: Index[_3D], indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[_3D]) extends DiscreteImageDomain[_3D] {

  private val inverseAnisotropicTransform = indexToPhysicalCoordinateTransform.inverse

  override def origin = indexToPhysicalCoordinateTransform(Point(0, 0, 0))

  override def spacing = Vector(iVecImage.norm.toFloat, jVecImage.norm.toFloat, kVecImage.norm.toFloat)

  private val iVecImage = indexToPhysicalCoordinateTransform(Point(1, 0, 0)) - indexToPhysicalCoordinateTransform(Point(0, 0, 0))
  private val jVecImage = indexToPhysicalCoordinateTransform(Point(0, 1, 0)) - indexToPhysicalCoordinateTransform(Point(0, 0, 0))
  private val kVecImage = indexToPhysicalCoordinateTransform(Point(0, 0, 1)) - indexToPhysicalCoordinateTransform(Point(0, 0, 0))

  val directions = SquareMatrix[_3D](
    ((iVecImage * (1.0 / iVecImage.norm)).data
      ++ (jVecImage * (1.0 / jVecImage.norm)).data
      ++ (kVecImage * (1.0 / kVecImage.norm)).data)
      .map(_.toFloat)
  )

  def points = for (k <- (0 until size(2)).toIterator; j <- (0 until size(1)).view; i <- (0 until size(0)).view)
    yield indexToPhysicalCoordinateTransform(Point(i, j, k))

  override def indexToPoint(i: Index[_3D]) = indexToPhysicalCoordinateTransform(Point(i(0), i(1), i(2)))

  override def index(pointId: Int) =
    Index(
      pointId % (size(0) * size(1)) % size(0),
      pointId % (size(0) * size(1)) / size(0),
      pointId / (size(0) * size(1)))

  override def pointId(idx: Index[_3D]): Int = {
    idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  }

  override def transform(t: Point[_3D] => Point[_3D]): UnstructuredPointsDomain[_3D] = {
    new UnstructuredPointsDomain3D(points.map(t).toIndexedSeq)
  }

}

/** Typeclass for creating domains of arbitrary dimensionality */
sealed trait CreateDiscreteImageDomain[D <: Dim] {
  def createImageDomain(origin: Point[D], spacing: Vector[D], size: Index[D]): DiscreteImageDomain[D]
  def createWithTransform(size: Index[D], transform: AnisotropicSimilarityTransformation[D]): DiscreteImageDomain[D]
}

object CreateDiscreteImageDomain {

  implicit object CreateDiscreteImageDomain1D extends CreateDiscreteImageDomain[_1D] {
    override def createImageDomain(origin: Point[_1D], spacing: Vector[_1D], size: Index[_1D]): DiscreteImageDomain[_1D] = {
      val rigidParameters = origin.data ++ Array(0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_1D](Point(0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
      new DiscreteImageDomain1D(size, anisotropSimTransform)

    }

    override def createWithTransform(size: Index[_1D], transform: AnisotropicSimilarityTransformation[_1D]): DiscreteImageDomain[_1D] = {
      new DiscreteImageDomain1D(size, transform)
    }
  }

  implicit object CreateDiscreteImageDomain2D extends CreateDiscreteImageDomain[_2D] {
    override def createImageDomain(origin: Point[_2D], spacing: Vector[_2D], size: Index[_2D]): DiscreteImageDomain[_2D] = {
      val rigidParameters = origin.data ++ Array(0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_2D](Point(0, 0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
      new DiscreteImageDomain2D(size, anisotropSimTransform)
    }

    override def createWithTransform(size: Index[_2D], transform: AnisotropicSimilarityTransformation[_2D]): DiscreteImageDomain[_2D] = new DiscreteImageDomain2D(size, transform)
  }

  implicit object CreateDiscreteImageDomain3D extends CreateDiscreteImageDomain[_3D] {
    override def createImageDomain(origin: Point[_3D], spacing: Vector[_3D], size: Index[_3D]): DiscreteImageDomain[_3D] = {
      val rigidParameters = origin.data ++ Array(0f, 0f, 0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
      new DiscreteImageDomain3D(size, anisotropSimTransform)
    }

    override def createWithTransform(size: Index[_3D], transform: AnisotropicSimilarityTransformation[_3D]): DiscreteImageDomain[_3D] = new DiscreteImageDomain3D(size, transform)
  }

}
