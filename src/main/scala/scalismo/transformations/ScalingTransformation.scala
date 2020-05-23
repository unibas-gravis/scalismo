package scalismo.transformations

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{EuclideanSpace, Field, RealSpace}
import scalismo.geometry
import scalismo.geometry.{_1D, _2D, _3D, EuclideanVector, NDSpace, Point, SquareMatrix}
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.TransformationSpace.{ParameterVector}

/**
 * D-dimensional translation transform that is parametric, invertible and differentiable
 *
 *  @param t Translation vector
 */
case class Scaling[D: NDSpace](s: Double)
    extends ParametricTransformation[D]
    with CanInvert[D, Scaling]
    with CanDifferentiateWRTPosition[D] {

  override val parameters = DenseVector[Double](s)

  override val f = (x: Point[D]) => (x.toVector * s).toPoint

  override val domain = EuclideanSpace[D]

  override val numberOfParameters: Int = 1

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = x => SquareMatrix.eye[D] * s

  override def inverse: Scaling[D] = {
    if (s == 0) Scaling[D](0) else Scaling[D](1.0 / s)
  }

  override def derivativeWRTParameters: JacobianField[D] = {
    val dim = NDSpace[D].dimensionality
    val jacobian = (x: Point[D]) => {
      val m = DenseMatrix.zeros[Double](dim, numberOfParameters)
      m(0 until dim, ::) := x.toBreezeVector
    }
    Field(domain, jacobian)
  }
}

object Scaling1D {
  def apply(s: Double): Scaling[_1D] = {
    new Scaling[_1D](s)
  }
}

object Scaling2D {
  def apply(s: Double): Scaling[_2D] = {
    new Scaling[_2D](s)
  }
}

object Scaling3D {
  def apply(s: Double): Scaling[_3D] = {
    new Scaling[_3D](s)
  }
}

case class ScalingSpace[D: NDSpace]() extends TransformationSpace[D] {

  override type T[D] = Scaling[D]
  override val domain = EuclideanSpace[D]

  def numberOfParameters: Int = implicitly[NDSpace[D]].dimensionality
  override def identityTransformation: Scaling[D] = {
    Scaling(1.0)
  }

  /**Returns a translation transform, where the translation vectors' coordinates are the given parameters*/
  override def transformationForParameters(p: ParameterVector): Scaling[D] = Scaling[D](p(0))
}

object ScalingSpace1D {
  def apply(): ScalingSpace[_1D] = ScalingSpace[_1D]()
}

object ScalingSpace2D {
  def apply(): ScalingSpace[_2D] = ScalingSpace[_2D]()
}

object ScalingSpace3D {
  def apply(): ScalingSpace[_3D] = ScalingSpace[_3D]()
}
