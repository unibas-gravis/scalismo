package org.statismo.stk.core
package image

import common.{ Domain, DiscreteDomain, BoxedDomain }
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.registration.CanInvert
import org.statismo.stk.core.registration.AnisotropicScalingTransformation3D
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.AnisotropicScalingSpace3D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformationSpace3D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation3D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation2D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformationSpace2D
import org.statismo.stk.core.registration.SimilarityTransformationSpace1D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformationSpace3D
import org.statismo.stk.core.registration.AnisotropicSimilarityTransformation3D

abstract class DiscreteImageDomain[D <: Dim] extends DiscreteDomain[D] with BoxedDomain[D] {

  def spacing: Vector[D]
  def size: Index[D]
  val directions: Array[Double]

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  def indexToLinearIndex(idx: Index[D]): Int
  def linearIndexToIndex(linearIdx: Int): Index[D]

  def indexToPoint(i: Index[D]): Point[D]
  def pointToIndex(p: Point[D]): Index[D]

  def isInside(pt: Point[D]): Boolean

}

case class DiscreteImageDomain1D(val origin: Point[OneD], val spacing: Vector[OneD], val size: Index[OneD]) extends DiscreteImageDomain[OneD] {

  val dimTraits = geometry.oneD

  def points = for (i <- (0 until size(0)).view) yield Point1D(origin(0) + spacing(0) * i) // TODO replace with operator version

  val extent: Point1D = Point1D(origin(0) + spacing(0) * size(0))

  def indexToLinearIndex(idx: Index[OneD]) = idx(0)
  def linearIndexToIndex(linearIdx: Int) = Index1D(linearIdx)

  val directions = Array(1.0)

  private val transform = SimilarityTransformationSpace1D().transformForParameters(DenseVector(origin.data ++ spacing.data))
  private val inverseTransform = transform.inverse
  
  override def indexToPoint(i: Index[OneD]): Point[OneD] = transform(Point1D(i(0)))
  override def pointToIndex(p: Point[OneD]): Index[OneD] = Index1D(inverseTransform(p)(0).toInt)

  def isInside(pt: Point[OneD]): Boolean = {
    pt(0) >= origin(0) && pt(0) <= extent(0)
  }

}

case class DiscreteImageDomain2D(size: Index[TwoD], anisotropSimTransform : AnisotropicSimilarityTransformation2D ) extends DiscreteImageDomain[TwoD] {

  val dimTraits = geometry.twoD

  private val inverseAnisotropicTransform = anisotropSimTransform.inverse
  
  def origin = anisotropSimTransform(Point2D(0, 0))

  private val iVecImage = anisotropSimTransform(Point2D(1, 0)) - anisotropSimTransform(Point2D(0, 0))
  private val jVecImage = anisotropSimTransform(Point2D(0, 1)) - anisotropSimTransform(Point2D(0, 0))

  override val directions = ((iVecImage * (1.0 / iVecImage.norm)).data ++ (jVecImage * (1.0 / jVecImage.norm)).data).map(_.toDouble)
  override def spacing = Vector2D(iVecImage.norm.toFloat, jVecImage.norm.toFloat)

  def points = for (j <- (0 until size(1)).view; i <- (0 until size(0)).view) yield anisotropSimTransform(Point2D(i, j))

  val extent = anisotropSimTransform(Point2D(size(0) - 1, size(1) - 1))

  def indexToLinearIndex(idx: Index[TwoD]) = idx(0) + idx(1) * size(0)
  def linearIndexToIndex(linearIdx: Int) = (Index2D(linearIdx % size(0), linearIdx / size(0)))

  override def indexToPoint(i: Index[TwoD]) = anisotropSimTransform(Point2D(i(0), i(1)))
  override def pointToIndex(p: Point[TwoD]) = {
    val t = inverseAnisotropicTransform(p).data.map(_.toInt)
    Index2D(t(0), t(1))
  }

  def isInside(pt: Point[TwoD]): Boolean = {
    val invPt = inverseAnisotropicTransform(pt)
    invPt(0) < size(0) && invPt(1) < size(1)
  }

}

case class DiscreteImageDomain3D(size: Index[ThreeD], anisotropSimTransform : AnisotropicSimilarityTransformation3D ) extends DiscreteImageDomain[ThreeD] {

  private val inverseAnisotropicTransform = anisotropSimTransform.inverse
  
  override def origin = anisotropSimTransform(Point3D(0, 0, 0))

  override def spacing = Vector3D(iVecImage.norm.toFloat, jVecImage.norm.toFloat, kVecImage.norm.toFloat)

  private val iVecImage = anisotropSimTransform(Point3D(1, 0, 0)) - anisotropSimTransform(Point3D(0, 0, 0))
  private val jVecImage = anisotropSimTransform(Point3D(0, 1, 0)) - anisotropSimTransform(Point3D(0, 0, 0))
  private val kVecImage = anisotropSimTransform(Point3D(0, 0, 1)) - anisotropSimTransform(Point3D(0, 0, 0))

  val directions = ((iVecImage * (1.0 / iVecImage.norm)).data ++ (jVecImage * (1.0 / jVecImage.norm)).data ++ (kVecImage * (1.0 / kVecImage.norm)).data).map(_.toDouble)

  val dimTraits = geometry.threeD

  def points = for (k <- (0 until size(2)).view; j <- (0 until size(1)).view; i <- (0 until size(0)).view)
    yield anisotropSimTransform(Point3D(i, j, k))

  override def indexToPoint(i: Index[ThreeD]) = anisotropSimTransform(Point3D(i(0), i(1), i(2)))
  override def pointToIndex(p: Point[ThreeD]) = {
    val t = inverseAnisotropicTransform(p).data.map(_.toInt)
    Index3D(t(0), t(1), t(2))
  }

  val extent = anisotropSimTransform(Point3D(size(0) - 1, size(1) - 1, size(2) - 1))
  def indexToLinearIndex(idx: Index[ThreeD]) = idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  def linearIndexToIndex(linearIdx: Int) =
    Index3D(
      linearIdx % (size(0) * size(1)) % size(0),
      linearIdx % (size(0) * size(1)) / size(0),
      linearIdx / (size(0) * size(1)))

  def isInside(pt: Point[ThreeD]): Boolean = {
    val invPt = inverseAnisotropicTransform(pt)
    invPt(0) < size(0) && invPt(1) < size(1) && invPt(2) < size(2)
  }
}

object DiscreteImageDomain3D {
  def apply(origin: Point[ThreeD], spacing: Vector[ThreeD], size: Index[ThreeD]) = {

    val rigidParameters = origin.data ++ Array(0f, 0f, 0f)
    val anisotropicScalingParmaters = spacing.data
    val anisotropSimTransform = AnisotropicSimilarityTransformationSpace3D().transformForParameters( DenseVector(rigidParameters++anisotropicScalingParmaters))
    new DiscreteImageDomain3D(size,anisotropSimTransform)

  }

}

object DiscreteImageDomain2D {
  def apply(origin: Point[TwoD], spacing: Vector[TwoD], size: Index[TwoD]) = {
    val rigidParameters = origin.data ++ Array(0f)
    val anisotropicScalingParmaters = spacing.data    
    val anisotropSimTransform = AnisotropicSimilarityTransformationSpace2D().transformForParameters( DenseVector(rigidParameters++anisotropicScalingParmaters))
    new DiscreteImageDomain2D(size, anisotropSimTransform)
  }
}




