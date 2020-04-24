package scalismo.transformations

import breeze.linalg.DenseVector
import scalismo.common.{Domain, EuclideanSpace}
import scalismo.geometry.{_1D, _2D, _3D, NDSpace, Point, SquareMatrix}
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.TransformationSpace.ParameterVector

case class SimilarityTransformation[D](scaling: Scaling[D],
                                       rigidTranform: RigidTransformation[D],
                                       isInverse: Boolean = false)
    extends ParametricTransformation[D]
    with CanDifferentiate[D]
    with CanInvert[D] {

  override val domain: Domain[D] = EuclideanSpace[D]

  private val compositeTransformation = if (!isInverse) {
    CompositeTransformation(scaling, rigidTranform)
  } else {
    CompositeTransformation(rigidTranform, scaling)
  }

  override val numberOfParameters = compositeTransformation.numberOfParameters
  override val parameters = compositeTransformation.parameters
  override val jacobian = compositeTransformation.jacobian

  override def inverse: SimilarityTransformation[D] = {
    SimilarityTransformation(scaling.inverse, rigidTranform.inverse, !isInverse)
  }

  /** Derivative of the transform evaluated at a point */
  override def derivative: Point[D] => SquareMatrix[D] = compositeTransformation.derivative
  override def f: Point[D] => Point[D] = compositeTransformation.f
}

object SimilarityTransformation3D {
  def apply(scaling: Scaling[_3D], rigidTransformation: RigidTransformation[_3D]): SimilarityTransformation[_3D] = {
    SimilarityTransformation(scaling, rigidTransformation, false)
  }
}

object SimilarityTransformation2D {
  def apply(scaling: Scaling[_2D], rigidTransformation: RigidTransformation[_2D]): SimilarityTransformation[_2D] = {
    SimilarityTransformation(scaling, rigidTransformation, false)
  }
}

trait SimilarityTransformationSpace[D] extends TransformationSpace[D] {

  def scalingSpace: ScalingSpace[D]
  def rigidTransformationSpace: RigidTransformationSpace[D]

  override type T[D] = SimilarityTransformation[D]

  override def transformationForParameters(p: ParameterVector): SimilarityTransformation[D] = {
    val tparams = p(0 until scalingSpace.numberOfParameters)
    val rparams = p(rigidTransformationSpace.numberOfParameters until p.length)
    SimilarityTransformation(scalingSpace.transformationForParameters(tparams),
                             rigidTransformationSpace.transformationForParameters(rparams))
  }

  override val domain = rigidTransformationSpace.domain
  override val numberOfParameters = scalingSpace.numberOfParameters + rigidTransformationSpace.numberOfParameters

}

/**
 * Trait for D-dimensional anisotropic similarity transform that is a combination of a rigid transform and anisotropic scaling.
 *
 * There are different possibilities to define such a similarity transform. Either we first do a rigid transform and then scaling,
 * or vice versa. We support only one way where we scale first, then transform rigidly.
 *
 * The order of the rigid transform in this case is also fixed : first rotate then translate.
 *
 */
trait AnisotropicSimilarityTransformation[D]
    extends ParametricTransformation[D]
    with CanDifferentiate[D]
    with CanInvert[D] {

  override def inverse: AnisotropicSimilarityTransformation[D]
}

private case class RigidTransformationThenAnisotropicScaling[D: NDSpace](
  anisotropicScaling: AnisotropicScalingTransformation[D],
  rigidTransform: RigidTransformation[D]
) extends AnisotropicSimilarityTransformation[D] {

  private val compositeTranform = CompositeTransformation(anisotropicScaling, rigidTransform)

  override def inverse: AnisotropicSimilarityTransformation[D] =
    new AnisotropicScalingThenRigidTransformation[D](rigidTransform.inverse, anisotropicScaling.inverse)

  override def numberOfParameters: Int = compositeTranform.numberOfParameters

  /** Derivative of the transform evaluated at a point */
  override def derivative: Point[D] => SquareMatrix[D] = compositeTranform.derivative

  override def parameters: DenseVector[Double] = compositeTranform.parameters

  override def jacobian: JacobianField[D] = compositeTranform.jacobian

  override def domain: Domain[D] = compositeTranform.domain

  override def f: Point[D] => Point[D] = compositeTranform.f
}

private case class AnisotropicScalingThenRigidTransformation[D: NDSpace](
  rigidTransform: RigidTransformation[D],
  anisotropicScaling: AnisotropicScalingTransformation[D]
) extends AnisotropicSimilarityTransformation[D] {

  private val compositeTranform = CompositeTransformation(anisotropicScaling, rigidTransform)

  override def inverse: AnisotropicSimilarityTransformation[D] =
    new RigidTransformationThenAnisotropicScaling[D](anisotropicScaling.inverse, rigidTransform.inverse)

  /** Derivative of the transform evaluated at a point */
  override def derivative: Point[D] => SquareMatrix[D] = compositeTranform.derivative

  override def parameters: DenseVector[Double] = compositeTranform.parameters

  override def numberOfParameters: Int = compositeTranform.numberOfParameters

  override def jacobian: JacobianField[D] = compositeTranform.jacobian

  override def domain: Domain[D] = compositeTranform.domain

  override def f: Point[D] => Point[D] = compositeTranform.f
}

object AnisotropicSimilarityTransformation1D {
  def apply(scaling: AnisotropicScalingTransformation[_1D],
            rigidTransformation: RigidTransformation[_1D]): AnisotropicSimilarityTransformation[_1D] = {
    RigidTransformationThenAnisotropicScaling(scaling, rigidTransformation)
  }
}

object AnisotropicSimilarityTransformation2D {
  def apply(scaling: AnisotropicScalingTransformation[_2D],
            rigidTransformation: RigidTransformation[_2D]): AnisotropicSimilarityTransformation[_2D] = {
    RigidTransformationThenAnisotropicScaling(scaling, rigidTransformation)
  }
}

object AnisotropicSimilarityTransformation3D {
  def apply(scaling: AnisotropicScalingTransformation[_3D],
            rigidTransformation: RigidTransformation[_3D]): AnisotropicSimilarityTransformation[_3D] = {
    RigidTransformationThenAnisotropicScaling(scaling, rigidTransformation)
  }
}

/**
 * Parametric transformations space producing anisotropic similarity transforms.
 *
 * @constructor Returns a parametric space generating anisotropic similarity transforms
 * @param center : center of rotation used in the rigid transform
 */
trait AnisotropicSimilarityTransformationSpace[D] extends TransformationSpaceWithDifferentiableTransforms[D] {
  def center: Point[D]
}

case class AnisotropicSimilarityTransformationSpace1D(center: Point[_1D])
    extends AnisotropicSimilarityTransformationSpace[_1D] {

  override type T[_1D] = AnisotropicSimilarityTransformation[_1D]
  private val productSpace = ProductTransformationSpace(AnisotropicScalingSpace3D(), RigidTransformationSpace3D(center))

  override def domain: Domain[_1D] = productSpace.domain

  override def numberOfParameters: Int = productSpace.numberOfParameters

  override def transformationForParameters(p: ParameterVector): AnisotropicSimilarityTransformation[_1D] = {
    val ct = productSpace.transformationForParameters(p)
    AnisotropicSimilarityTransformation1D(ct.outerTransform, ct.innerTransform)
  }

  /** returns identity transformation) */
  override def identityTransformation: AnisotropicSimilarityTransformation[_1D] = {
    AnisotropicSimilarityTransformation1D(AnisotropicScalingSpace3D().identityTransformation,
                                          RigidTransformationSpace3D(center).identityTransformation)
  }

}

case class AnisotropicSimilarityTranformationSpace2D(center: Point[_2D])
    extends AnisotropicSimilarityTransformationSpace[_2D] {

  override type T[_2D] = AnisotropicSimilarityTransformation[_2D]
  private val productSpace = ProductTransformationSpace(AnisotropicScalingSpace2D(), RigidTransformationSpace2D(center))

  override def domain: Domain[_2D] = productSpace.domain

  override def numberOfParameters: Int = productSpace.numberOfParameters

  override def transformationForParameters(p: ParameterVector): AnisotropicSimilarityTransformation[_2D] = {
    val ct = productSpace.transformationForParameters(p)
    AnisotropicSimilarityTransformation2D(ct.outerTransform, ct.innerTransform)
  }

  /** returns identity transformation) */
  override def identityTransformation: AnisotropicSimilarityTransformation[_2D] = {
    AnisotropicSimilarityTransformation2D(AnisotropicScalingSpace2D().identityTransformation,
                                          RigidTransformationSpace2D(center).identityTransformation)
  }

}

case class AnisotropicSimilarityTranformationSpace3D(center: Point[_3D])
    extends AnisotropicSimilarityTransformationSpace[_3D] {

  override type T[_3D] = AnisotropicSimilarityTransformation[_3D]
  private val productSpace = ProductTransformationSpace(AnisotropicScalingSpace3D(), RigidTransformationSpace3D(center))

  override def domain: Domain[_3D] = productSpace.domain

  override def numberOfParameters: Int = productSpace.numberOfParameters

  override def transformationForParameters(p: ParameterVector): AnisotropicSimilarityTransformation[_3D] = {
    val ct = productSpace.transformationForParameters(p)
    AnisotropicSimilarityTransformation3D(ct.outerTransform, ct.innerTransform)
  }

  /** returns identity transformation) */
  override def identityTransformation: AnisotropicSimilarityTransformation[_3D] = {
    AnisotropicSimilarityTransformation3D(AnisotropicScalingSpace3D().identityTransformation,
                                          RigidTransformationSpace3D(center).identityTransformation)
  }

}
