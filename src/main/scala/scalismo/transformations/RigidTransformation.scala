package scalismo.transformations

import breeze.linalg.DenseVector
import scalismo.common.{Domain, EuclideanSpace}
import scalismo.geometry.{
  _1D,
  _2D,
  _3D,
  EuclideanVector1D,
  EuclideanVector2D,
  EuclideanVector3D,
  NDSpace,
  Point,
  SquareMatrix
}
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.RigidTransformation.ApplicationOrder
import scalismo.transformations.RigidTransformation.ApplicationOrder.ApplicationOrder
import scalismo.transformations.TransformationSpace.ParameterVector

/**
 * Trait for D-dimensional rigid transform, that is a composition of rotation and translation transform.
 * Instances of this trait exist only for [[_2D]] and [[_3D]] as [[Rotation]] is not defined for [[_1D]]
 */
case class RigidTransformation[D](translation: Translation[D], rotation: Rotation[D], isInverse: Boolean = false)
    extends ParametricTransformation[D]
    with CanDifferentiate[D]
    with CanInvert[D] {

  override val domain: Domain[D] = EuclideanSpace[D]

  private val compositeTransformation = if (!isInverse) {
    CompositeTransformation(translation, rotation)
  } else {
    CompositeTransformation(rotation, translation)
  }

  override def parameters = compositeTransformation.parameters

  override def numberOfParameters: Int = compositeTransformation.numberOfParameters

  override def inverse: RigidTransformation[D] = {
    RigidTransformation(translation.inverse, rotation.inverse, !isInverse)
  }

  /** Derivative of the transform evaluated at a point */
  override def derivative: Point[D] => SquareMatrix[D] = compositeTransformation.derivative

  override def f: Point[D] => Point[D] = compositeTransformation.f

  override def jacobian: JacobianField[D] = compositeTransformation.jacobian
}

object RigidTransformation {
  object ApplicationOrder extends Enumeration {
    type ApplicationOrder = Value
    val ApplyRotationFirst, ApplyTranslationFirst = Value
  }
}

object RigidTransformation1D {
  def apply(translation: Translation[_1D]): RigidTransformation[_1D] = {
    RigidTransformation1D(translation, isInverse = true)
  }
}

object RigidTransformation2D {
  def apply(translation: Translation[_2D], rotation: Rotation[_2D]): RigidTransformation[_2D] = {
    RigidTransformation(translation, rotation, isInverse = true)
  }
}

object RigidTransformation3D {
  def apply(translation: Translation[_3D], rotation: Rotation[_3D]): RigidTransformation[_3D] = {
    RigidTransformation(translation, rotation, isInverse = false)
  }
}

/**
 * Parametric transformations space producing rigid transforms.
 */
trait RigidTransformationSpace[D] extends TransformationSpaceWithDifferentiableTransforms[D] {

  def translationSpace: TranslationSpace[D]
  def rotationSpace: RotationSpace[D]

  override type T[D] = RigidTransformation[D]

  override def transformationForParameters(p: ParameterVector) = {
    val tparams = p(0 until translationSpace.numberOfParameters)
    val rparams = p(translationSpace.numberOfParameters until p.length)
    RigidTransformation(translationSpace.transformationForParameters(tparams),
                        rotationSpace.transformationForParameters(rparams))
  }

  override val domain = rotationSpace.domain
  override val numberOfParameters = translationSpace.numberOfParameters + rotationSpace.numberOfParameters

}

case class RigidTransformationSpace1D(rotationCenter: Point[_1D]) extends RigidTransformationSpace[_1D] {
  val rotationSpace = RotationSpace1D(rotationCenter)
  val translationSpace = TranslationSpace1D()

  /** returns identity transformation) */
  override def identityTransformation: RigidTransformation[_1D] =
    RigidTransformation1D(Translation1D(EuclideanVector1D(0)), Rotation1D(0, rotationCenter))
}

case class RigidTransformationSpace1D(rotationCenter: Point[_1D]) extends RigidTransformationSpace[_1D] {
  val rotationSpace = RotationSpace1D(rotationCenter)
  val translationSpace = TranslationSpace1D()

  /** returns identity transformation) */
  override def identityTransformation: RigidTransformation[_1D] =
    RigidTransformation1D(Translation1D(EuclideanVector1D(0)), Rotation1D(0, rotationCenter))
}

case class RigidTransformationSpace3D(rotationCenter: Point[_3D]) extends RigidTransformationSpace[_3D] {
  val rotationSpace = RotationSpace3D(rotationCenter)
  val translationSpace = TranslationSpace3D()

  /** returns identity transformation) */
  override def identityTransformation: RigidTransformation[_3D] =
    RigidTransformation3D(Translation3D(EuclideanVector3D(0, 0, 0)), Rotation3D(0, 0, 0, rotationCenter))
}
