package org.statismo.stk.core
package image

import org.statismo.stk.core.common.{BoxDomain, FiniteDiscreteDomain}
import org.statismo.stk.core.geometry._
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformationSpace
import org.statismo.stk.core.registration.SimilarityTransformationSpace1D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation

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
abstract class DiscreteImageDomain[D <: Dim : NDSpace] extends FiniteDiscreteDomain[D]  with Equals {

  /** the first point (lower-left corner in 2D) of the grid */
  def origin : Point[D]

  /** the distance (in mm) between two points in each space direction */
  def spacing: Vector[D]

  /** the number of points in each direction */
  def size : Index[D]

  /** Direction cosine matrix */
  def directions: SquareMatrix[D]

  /** the dimensionality of the domain */
  val dimensionality = implicitly[NDSpace[D]].dimensionality

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  /** converts a grid index into a id that identifies a point */
  def pointId(idx: Index[D]): Int

  /** The index for the given point id */
  def index(pointId: Int): Index[D]

  /** the point corresponding to the given index */
  //def indexToPoint(i: Index[D]): Point[D]

  /** the index correspoinding to the physical point coordinate */
  //def pointToIndex(p: Point[D]): Index[D]


  /** a rectangular region that represents the area over which an image is defined by the points
    * that represent this image.
    */
  def imageBox : BoxDomain[D] = {
    val extendData = (0 until dimensionality).map(i => size(i) * spacing(i))
    val extent = Vector[D](extendData.toArray)
    val oppositeCorner = origin + extent
    BoxDomain(origin, oppositeCorner)
  }

  /** true if the point is part of the grid points */
  override def isDefinedAt(pt : Point[D]) : Boolean = {
    isIndex(pointToContinuousIndex(pt))
  }

  /** returns the point id in case it is defined, None otherwise. */
  override def pointId(pt : Point[D]) : Option[Int] = {
    val cidx = pointToContinuousIndex(pt)
    val ptId = pointId(continuousIndextoIndex(cidx))
    if (isIndex(cidx)) Some(ptId) else None
  }


  private def continuousIndextoIndex(cidx : Vector[D]) : Index[D] = {
    val data = cidx.data.map(Math.round)
    Index[D](data)
  }
  private def pointToContinuousIndex(pt : Point[D]) : Vector[D] = {
    val data = (0 until dimensionality).map(i => (pt(i) - origin(i)) / spacing(i))
    Vector[D](data.toArray)
  }

  def indexToPoint(i: Index[D]) = {
    indexToPhysicalCoordinateTransform(Point[D](i.data.map(_.toFloat)))
  }


  private def isIndex(continousIndex : Vector[D]) : Boolean = {
    (0 until dimensionality).forall(i => (continousIndex(i) - Math.round(continousIndex(i)) < 1e-8))
  }

  /** the anisotropic similarity transform that maps between the index and physical coordinates*/
  private[core] def indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[D]



  // define the canEqual method
  override def canEqual(a: Any) = a.isInstanceOf[DiscreteImageDomain[D]]

  override def equals(a: Any) = {
    a match {
      // make sure we can compare the 2 objects
      case c: DiscreteImageDomain[D] => {
        c.canEqual(this) &&
        origin == c.origin &&
        spacing == c.spacing &&
        size == c.size
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

  /** Typeclass for creating domains of arbitrary dimensionality */
  sealed trait CanCreate[D <: Dim] {
    def createImageDomain(origin: Point[D], spacing: Vector[D], size: Index[D]): DiscreteImageDomain[D]
    def createWithTransform(size: Index[D], transform: AnisotropicSimilarityTransformation[D]): DiscreteImageDomain[D]
  }

  implicit object canCreateImageDomain2D extends CanCreate[_2D] {
    override def createImageDomain(origin: Point[_2D], spacing: Vector[_2D], size: Index[_2D]): DiscreteImageDomain[_2D] = {
      val rigidParameters = origin.data ++ Array(0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_2D](Point(0, 0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
       new DiscreteImageDomain2D(size, anisotropSimTransform)      
    }
    override def createWithTransform(size: Index[_2D], transform: AnisotropicSimilarityTransformation[_2D]): DiscreteImageDomain[_2D] = new DiscreteImageDomain2D(size, transform)
  }

  implicit object canCreateImageDomain3D extends CanCreate[_3D] {
    override def createImageDomain(origin: Point[_3D], spacing: Vector[_3D], size: Index[_3D]): DiscreteImageDomain[_3D] = {
      val rigidParameters = origin.data ++ Array(0f, 0f, 0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
      new DiscreteImageDomain3D(size, anisotropSimTransform)
    }
    override def createWithTransform(size: Index[_3D], transform: AnisotropicSimilarityTransformation[_3D]): DiscreteImageDomain[_3D] = new DiscreteImageDomain3D(size, transform)
  }

   implicit object canCreateImageDomain1D extends CanCreate[_1D] {
    override def createImageDomain(origin: Point[_1D], spacing: Vector[_1D], size: Index[_1D]): DiscreteImageDomain[_1D] = new DiscreteImageDomain1D(origin, spacing, size)
    override def createWithTransform(size: Index[_1D], transform: AnisotropicSimilarityTransformation[_1D]): DiscreteImageDomain[_1D] = {
      val origin = transform(Point(0))
      val spacing = transform(Point(1)) - origin
      new DiscreteImageDomain1D(origin, spacing, size)
    }
  }

  /** Create a new discreteImageDomain with given origin, spacing and size*/
  def apply[D <: Dim](origin: Point[D], spacing: Vector[D], size: Index[D])(implicit evDim: NDSpace[D], evCreate: CanCreate[D]) = {
    evCreate.createImageDomain(origin, spacing, size)
  }

  /** Create a discreteImageDomain where the points are defined as tranformations of the indeces (from (0,0,0) to (size - 1, size - 1 , size -1)
    * This makes it possible to define image regions which are not aligned to the coordinate axis.
    */
  private[core] def apply[D <: Dim](size: Index[D], transform: AnisotropicSimilarityTransformation[D])(implicit evDim: NDSpace[D], evCreateRot: CanCreate[D]) = {
    evCreateRot.createWithTransform(size, transform)
  }

}


//
// The actual implementations for each dimension
//
private case class DiscreteImageDomain1D(origin: Point[_1D], spacing: Vector[_1D], size: Index[_1D]) extends DiscreteImageDomain[_1D] {

  def points = for (i <- (0 until size(0)).toIterator) yield Point(origin(0) + spacing(0) * i) // TODO replace with operator version

  override def indexToPhysicalCoordinateTransform = transform

  override def index(linearIdx: Int) = Index(linearIdx)
  override def pointId(idx : Index[_1D]) = idx(0)

  override val directions = SquareMatrix(1.0f)

  private val transform = SimilarityTransformationSpace1D().transformForParameters(DenseVector(origin.data ++ spacing.data))
  private val inverseTransform = transform.inverse

//  override def indexToPoint(i: Index[_1D]): Point[_1D] = transform(Point(i(0)))
//  override def pointToIndex(p: Point[_1D]): Index[_1D] = Index(inverseTransform(p)(0).toInt)

}

private case class DiscreteImageDomain2D(size: Index[_2D], indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[_2D]) extends DiscreteImageDomain[_2D] {

  private val inverseAnisotropicTransform = indexToPhysicalCoordinateTransform.inverse

  def origin = indexToPhysicalCoordinateTransform(Point(0, 0))

  private val iVecImage = indexToPhysicalCoordinateTransform(Point(1, 0)) - indexToPhysicalCoordinateTransform(Point(0, 0))
  private val jVecImage = indexToPhysicalCoordinateTransform(Point(0, 1)) - indexToPhysicalCoordinateTransform(Point(0, 0))

  private val nomiVecImage = iVecImage * (1.0 / iVecImage.norm)
  private val nomjVecImage = jVecImage * (1.0 / jVecImage.norm)

  if (Math.abs(nomiVecImage(1)) > 0.001f || Math.abs(nomjVecImage(0)) > 0.001f)
    throw new NotImplementedError(s"DiscreteImageDomain needs to be oriented along the space axis in this version. Image directions : i:${nomiVecImage} j:${nomjVecImage}")

  override val directions = SquareMatrix[_2D]((iVecImage * (1.0 / iVecImage.norm)).data ++ (jVecImage * (1.0 / jVecImage.norm)).data)
  override def spacing = Vector(iVecImage.norm.toFloat, jVecImage.norm.toFloat)

  def points = for (j <- (0 until size(1)).toIterator; i <- (0 until size(0)).view) yield indexToPhysicalCoordinateTransform(Point(i, j))

//  override def pointToIndex(p: Point[_2D]) = {
//    val t = inverseAnisotropicTransform(p).data.map(_.toInt)
//    Index(t(0), t(1))
//  }

  override def index(ptId: Int) = (Index(ptId % size(0), ptId / size(0)))
  override def pointId(idx: Index[_2D]) = idx(0) + idx(1) * size(0)

}

private case class DiscreteImageDomain3D(size: Index[_3D], indexToPhysicalCoordinateTransform: AnisotropicSimilarityTransformation[_3D]) extends DiscreteImageDomain[_3D] {

  private val inverseAnisotropicTransform = indexToPhysicalCoordinateTransform.inverse

  override def origin = indexToPhysicalCoordinateTransform(Point(0, 0, 0))

  override def spacing = Vector(iVecImage.norm.toFloat, jVecImage.norm.toFloat, kVecImage.norm.toFloat)

  private val iVecImage = indexToPhysicalCoordinateTransform(Point(1, 0, 0)) - indexToPhysicalCoordinateTransform(Point(0, 0, 0))
  private val jVecImage = indexToPhysicalCoordinateTransform(Point(0, 1, 0)) - indexToPhysicalCoordinateTransform(Point(0, 0, 0))
  private val kVecImage = indexToPhysicalCoordinateTransform(Point(0, 0, 1)) - indexToPhysicalCoordinateTransform(Point(0, 0, 0))

  private val nomiVecImage = iVecImage * (1.0 / iVecImage.norm)
  private val nomjVecImage = jVecImage * (1.0 / jVecImage.norm)
  private val nomkVecImage = kVecImage * (1.0 / kVecImage.norm)

  /**
   * To be removed after refactoring : we make sure that there is no rotation of the image domain in order to remain coherent with
   * the BoxedDomain implmentation that is assuming directions along the space axis.
   */

  if (Math.abs(nomiVecImage(1)) > 0.06f || Math.abs(nomiVecImage(2)) > 0.06f || Math.abs(nomjVecImage(0)) > 0.06f || Math.abs(nomjVecImage(2)) > 0.06f || Math.abs(nomkVecImage(0)) > 0.06f || Math.abs(nomkVecImage(1)) > 0.06f)
    throw new NotImplementedError(s"DiscreteImageDomain needs to be oriented along the space axis in this version. Image directions : i:${nomiVecImage} j:${nomjVecImage} k:${nomkVecImage}")

  val directions = SquareMatrix[_3D](
    ((iVecImage * (1.0 / iVecImage.norm)).data
    ++ (jVecImage * (1.0 / jVecImage.norm)).data
    ++ (kVecImage * (1.0 / kVecImage.norm)).data)
      .map(_.toFloat)
  )

  def points = for (k <- (0 until size(2)).toIterator; j <- (0 until size(1)).view; i <- (0 until size(0)).view)
  yield indexToPhysicalCoordinateTransform(Point(i, j, k))

  override def indexToPoint(i: Index[_3D]) = indexToPhysicalCoordinateTransform(Point(i(0), i(1), i(2)))
//  override def pointToIndex(p: Point[_3D]) = {
//    val t = inverseAnisotropicTransform(p).data.map(_.toInt)
//      Index(t(0), t(1), t(2))
//  }

  override def index(pointId: Int) =
    Index(
      pointId % (size(0) * size(1)) % size(0),
      pointId % (size(0) * size(1)) / size(0),
      pointId / (size(0) * size(1)))


  override def pointId(idx : Index[_3D]) : Int = {
    idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  }

}



