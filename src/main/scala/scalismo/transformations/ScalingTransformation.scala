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
    with CanInvert[D]
    with CanDifferentiate[D] {

  override val parameters = DenseVector[Double](s)

  override val f = (x: Point[D]) => (x.toVector * s).toPoint

  override val domain = EuclideanSpace[D]

  override val numberOfParameters: Int = 1

  override def derivative: Point[D] => SquareMatrix[D] = x => SquareMatrix.eye[D] * s

  override def inverse: Scaling[D] = {
    if (s == 0) Scaling[D](0) else Scaling[D](1.0 / s)
  }

  override def jacobian: JacobianField[D] = {
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

/**
 * Anisotropic scaling transform, where each dimension is scaled differently
 *
 *  @constructor creates a D-dimensional anisotropic scaling transform
 *  @param s Vector of the same dimensionality as the space indicating for each dimension the scaling factor
 */
case class AnisotropicScalingTransformation[D: NDSpace](s: geometry.EuclideanVector[D])
    extends ParametricTransformation[D]
    with CanInvert[D]
    with CanDifferentiate[D] {
  override val domain = EuclideanSpace[D]
  override val f = (x: Point[D]) => Point((x.toVector.toBreezeVector *:* s.toBreezeVector).data)

  val parameters = s.toBreezeVector
  override val numberOfParameters = NDSpace[D].dimensionality

  def derivative: Point[D] => SquareMatrix[D] = x => SquareMatrix[D](breeze.linalg.diag(s.toBreezeVector).data)

  override def inverse: AnisotropicScalingTransformation[D] = {
    val sinv = s.toArray.map(v => if (v == 0) 0.0 else 1.0 / v)
    new AnisotropicScalingTransformation[D](EuclideanVector[D](sinv))
  }

  override def jacobian: JacobianField[D] = {
    val jacobian = (x: Point[D]) => new DenseMatrix(numberOfParameters, 1, x.toArray)
    Field(domain, jacobian)
  }
}

/**
 * Parametric transformations space producing anisotropic scaling transforms.
 *
 * @constructor Returns a D-dimensional anisotropic scaling space
 */
case class AnisotropicScalingSpace[D: NDSpace]() extends TransformationSpaceWithDifferentiableTransforms[D] {

  override val domain = EuclideanSpace[D]
  override type T[D] = AnisotropicScalingTransformation[D]
  def numberOfParameters: Int = implicitly[NDSpace[D]].dimensionality

  override def identityTransformation = {
    transformationForParameters(DenseVector.ones[Double](numberOfParameters))
  }

  /**
   * Returns a D-dimensional anisotropic scaling transform corresponding to the indicated parameters
   *
   *  @param p Scaling factor for each dimension. Must be of the same dimensionality as the space.
   */
  override def transformationForParameters(p: ParameterVector): AnisotropicScalingTransformation[D] = {
    require(p.length == numberOfParameters)
    AnisotropicScalingTransformation[D](EuclideanVector(p.data.take(numberOfParameters)))
  }

}

object AnisotropicScalingSpace3D {
  def apply(): AnisotropicScalingSpace[_3D] = AnisotropicScalingSpace3D()
}

object AnisotropicScalingSpace2D {
  def apply(): AnisotropicScalingSpace[_2D] = AnisotropicScalingSpace2D()
}
