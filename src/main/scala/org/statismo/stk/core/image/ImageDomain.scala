package org.statismo.stk.core
package image

import common.{ DiscreteDomain, BoxedDomain }
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.registration.CanInvert
import org.statismo.stk.core.registration.AnisotropicScalingTransformation
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.AnisotropicScalingSpace
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformationSpace
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D
import org.statismo.stk.core.registration.SimilarityTransformationSpace1D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation
import org.statismo.stk.core.registration.AnisotropicScalingTransformation
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation
import org.statismo.stk.core.common.FiniteDiscreteDomain

abstract class DiscreteImageDomain[D <: Dim] extends FiniteDiscreteDomain[D] with BoxedDomain[D] {

  def spacing: Vector[D]
  def size: Index[D]
  val directions: Array[Double]

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  def indexToLinearIndex(idx: Index[D]): Int
  def linearIndexToIndex(linearIdx: Int): Index[D]

  def indexToPoint(i: Index[D]): Point[D]
  def pointToIndex(p: Point[D]): Index[D]

  def isInside(pt: Point[D]): Boolean
  def anisotropSimTransform: AnisotropicSimilarityTransformation[D]

}

private case class DiscreteImageDomain1D(val origin: Point[_1D], val spacing: Vector[_1D], val size: Index[_1D]) extends DiscreteImageDomain[_1D] {

  def points = for (i <- (0 until size(0)).toIterator) yield Point(origin(0) + spacing(0) * i) // TODO replace with operator version

  val extent: Point[_1D] = Point(origin(0) + spacing(0) * size(0))

  override def anisotropSimTransform = transform
  def indexToLinearIndex(idx: Index[_1D]) = idx(0)
  def linearIndexToIndex(linearIdx: Int) = Index(linearIdx)

  val directions = Array(1.0)

  private val transform = SimilarityTransformationSpace1D().transformForParameters(DenseVector(origin.data ++ spacing.data))
  private val inverseTransform = transform.inverse

  override def indexToPoint(i: Index[_1D]): Point[_1D] = transform(Point(i(0)))
  override def pointToIndex(p: Point[_1D]): Index[_1D] = Index(inverseTransform(p)(0).toInt)

  def isInside(pt: Point[_1D]): Boolean = {
    pt(0) >= origin(0) && pt(0) <= extent(0)
  }

}

private case class DiscreteImageDomain2D(size: Index[_2D], anisotropSimTransform: AnisotropicSimilarityTransformation[_2D]) extends DiscreteImageDomain[_2D] {

  private val inverseAnisotropicTransform = anisotropSimTransform.inverse

  def origin = anisotropSimTransform(Point(0, 0))

  private val iVecImage = anisotropSimTransform(Point(1, 0)) - anisotropSimTransform(Point(0, 0))
  private val jVecImage = anisotropSimTransform(Point(0, 1)) - anisotropSimTransform(Point(0, 0))

  private val nomiVecImage = iVecImage * (1.0 / iVecImage.norm)
  private val nomjVecImage = jVecImage * (1.0 / jVecImage.norm)

  if (Math.abs(nomiVecImage(1)) > 0.001f || Math.abs(nomjVecImage(0)) > 0.001f)
    throw new NotImplementedError(s"DiscreteImageDomain needs to be oriented along the space axis in this version. Image directions : i:${nomiVecImage} j:${nomjVecImage}")

  override val directions = ((iVecImage * (1.0 / iVecImage.norm)).data ++ (jVecImage * (1.0 / jVecImage.norm)).data).map(_.toDouble)
  override def spacing = Vector(iVecImage.norm.toFloat, jVecImage.norm.toFloat)

  def points = for (j <- (0 until size(1)).toIterator; i <- (0 until size(0)).view) yield anisotropSimTransform(Point(i, j))

  val extent = anisotropSimTransform(Point(size(0) - 1, size(1) - 1))

  override def indexToPoint(i: Index[_2D]) = anisotropSimTransform(Point(i(0), i(1)))
  override def pointToIndex(p: Point[_2D]) = {
    val t = inverseAnisotropicTransform(p).data.map(_.toInt)
    Index(t(0), t(1))
  }

  def indexToLinearIndex(idx: Index[_2D]) = idx(0) + idx(1) * size(0)
  def linearIndexToIndex(linearIdx: Int) = (Index(linearIdx % size(0), linearIdx / size(0)))

  def isInside(pt: Point[_2D]): Boolean = {
    val invPt = inverseAnisotropicTransform(pt)
    invPt(0) < size(0) && invPt(1) < size(1)
  }

}

private case class DiscreteImageDomain3D(size: Index[_3D], anisotropSimTransform: AnisotropicSimilarityTransformation[_3D]) extends DiscreteImageDomain[_3D] {

  private val inverseAnisotropicTransform = anisotropSimTransform.inverse

  override def origin = anisotropSimTransform(Point(0, 0, 0))

  override def spacing = Vector(iVecImage.norm.toFloat, jVecImage.norm.toFloat, kVecImage.norm.toFloat)

  private val iVecImage = anisotropSimTransform(Point(1, 0, 0)) - anisotropSimTransform(Point(0, 0, 0))
  private val jVecImage = anisotropSimTransform(Point(0, 1, 0)) - anisotropSimTransform(Point(0, 0, 0))
  private val kVecImage = anisotropSimTransform(Point(0, 0, 1)) - anisotropSimTransform(Point(0, 0, 0))

  private val nomiVecImage = iVecImage * (1.0 / iVecImage.norm)
  private val nomjVecImage = jVecImage * (1.0 / jVecImage.norm)
  private val nomkVecImage = kVecImage * (1.0 / kVecImage.norm)

  /**
   * To be removed after refactoring : we make sure that there is no rotation of the image domain in order to remain coherent with
   * the BoxedDomain implmentation that is assuming directions along the space axis.
   */

  if (Math.abs(nomiVecImage(1)) > 0.06f || Math.abs(nomiVecImage(2)) > 0.06f || Math.abs(nomjVecImage(0)) > 0.06f || Math.abs(nomjVecImage(2)) > 0.06f || Math.abs(nomkVecImage(0)) > 0.06f || Math.abs(nomkVecImage(1)) > 0.06f)
    throw new NotImplementedError(s"DiscreteImageDomain needs to be oriented along the space axis in this version. Image directions : i:${nomiVecImage} j:${nomjVecImage} k:${nomkVecImage}")

  val directions = ((iVecImage * (1.0 / iVecImage.norm)).data ++ (jVecImage * (1.0 / jVecImage.norm)).data ++ (kVecImage * (1.0 / kVecImage.norm)).data).map(_.toDouble)

  def points = for (k <- (0 until size(2)).toIterator; j <- (0 until size(1)).view; i <- (0 until size(0)).view)
    yield anisotropSimTransform(Point(i, j, k))

  override def indexToPoint(i: Index[_3D]) = anisotropSimTransform(Point(i(0), i(1), i(2)))
  override def pointToIndex(p: Point[_3D]) = {
    val t = inverseAnisotropicTransform(p).data.map(_.toInt)
    Index(t(0), t(1), t(2))
  }

  val extent = anisotropSimTransform(Point(size(0) - 1, size(1) - 1, size(2) - 1))
  def indexToLinearIndex(idx: Index[_3D]) = idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  def linearIndexToIndex(linearIdx: Int) =
    Index(
      linearIdx % (size(0) * size(1)) % size(0),
      linearIdx % (size(0) * size(1)) / size(0),
      linearIdx / (size(0) * size(1)))

  def isInside(pt: Point[_3D]): Boolean = {
    val invPt = inverseAnisotropicTransform(pt)
    invPt(0) < size(0) && invPt(1) < size(1) && invPt(2) < size(2)
  }

}

object DiscreteImageDomain {

  trait Create[D <: Dim] {
    def createImageDomain(origin: Point[D], spacing: Vector[D], size: Index[D]): DiscreteImageDomain[D]
    def createWithTransform(size: Index[D], transform: AnisotropicSimilarityTransformation[D]): DiscreteImageDomain[D]
  }

  implicit object createImageDomain2D extends Create[_2D] {
    override def createImageDomain(origin: Point[_2D], spacing: Vector[_2D], size: Index[_2D]): DiscreteImageDomain[_2D] = {
      val rigidParameters = origin.data ++ Array(0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_2D](Point(0, 0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
       new DiscreteImageDomain2D(size, anisotropSimTransform)      
    }
    override def createWithTransform(size: Index[_2D], transform: AnisotropicSimilarityTransformation[_2D]): DiscreteImageDomain[_2D] = new DiscreteImageDomain2D(size, transform)
  }

  implicit object createImageDomain3D extends Create[_3D] {
    override def createImageDomain(origin: Point[_3D], spacing: Vector[_3D], size: Index[_3D]): DiscreteImageDomain[_3D] = {
      val rigidParameters = origin.data ++ Array(0f, 0f, 0f)
      val anisotropicScalingParmaters = spacing.data
      val anisotropSimTransform = AnisotropicSimilarityTransformationSpace[_3D](Point(0, 0, 0)).transformForParameters(DenseVector(rigidParameters ++ anisotropicScalingParmaters))
      new DiscreteImageDomain3D(size, anisotropSimTransform)
    }
    override def createWithTransform(size: Index[_3D], transform: AnisotropicSimilarityTransformation[_3D]): DiscreteImageDomain[_3D] = new DiscreteImageDomain3D(size, transform)
  }

  implicit object createImageDomain1D extends Create[_1D] {
    override def createImageDomain(origin: Point[_1D], spacing: Vector[_1D], size: Index[_1D]): DiscreteImageDomain[_1D] = new DiscreteImageDomain1D(origin, spacing, size)
    override def createWithTransform(size: Index[_1D], transform: AnisotropicSimilarityTransformation[_1D]): DiscreteImageDomain[_1D] = {
      val origin = transform(Point(0))
      val spacing = transform(Point(1)) - origin
      new DiscreteImageDomain1D(origin, spacing, size)
    }
  }

  def apply[D <: Dim](origin: Point[D], spacing: Vector[D], size: Index[D])(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    evCreateRot.createImageDomain(origin, spacing, size)
  }

  def apply[D <: Dim](size: Index[D], transform: AnisotropicSimilarityTransformation[D])(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    evCreateRot.createWithTransform(size, transform)
  }

}






