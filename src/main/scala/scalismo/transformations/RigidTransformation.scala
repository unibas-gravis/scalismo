package scalismo.transformations

import breeze.linalg.DenseVector
import scalismo.common.{Domain, EuclideanSpace, EuclideanSpace2D, EuclideanSpace3D}
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
import scalismo.transformations.TransformationSpace.ParameterVector

/**
 * Trait for D-dimensional rigid transform, that is a composition of rotation and translation transform.
 * Instances of this trait exist only for [[_2D]] and [[_3D]] as [[Rotation]] is not defined for [[_1D]]
 */
trait RigidTransformation[D]
    extends SimilarityTransformation[D]
    with CanDifferentiateWRTPosition[D]
    with CanInvert[D, RigidTransformation] {}

case class RotationThenTranslation[D](rotation: Rotation[D], translation: Translation[D])
    extends RigidTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(translation, rotation)

  override def inverse = TranslationThenRotation(translation.inverse, rotation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f
}

object RotationThenTranslation2D {
  def apply(rotation: Rotation[_2D], translation: Translation[_2D]): RotationThenTranslation[_2D] = {
    RotationThenTranslation(rotation, translation)
  }
}

object RotationThenTranslation3D {
  def apply(rotation: Rotation[_3D], translation: Translation[_3D]): RotationThenTranslation[_3D] = {
    RotationThenTranslation(rotation, translation)
  }
}

case class TranslationThenRotation[D](translation: Translation[D], rotation: Rotation[D])
    extends RigidTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(rotation, translation)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f

  override def inverse = RotationThenTranslation(rotation.inverse, translation.inverse)
}

object TranslationThenRotation2D {
  def apply(translation: Translation[_2D], rotation: Rotation[_2D]): TranslationThenRotation[_2D] = {
    TranslationThenRotation(translation, rotation)
  }
}

object TranslationThenRotation3D {
  def apply(translation: Translation[_3D], rotation: Rotation[_3D]): TranslationThenRotation[_3D] = {
    TranslationThenRotation(translation, rotation)
  }
}

case class RotationThenTranslationSpace2D(rotationCenter: Point[_2D])
    extends TransformationSpaceWithDifferentiableTransforms[_2D] {

  private val productTS = ProductTransformationSpace(TranslationSpace2D, RotationSpace2D(rotationCenter))

  override type T[D] = RotationThenTranslation[D]

  override def domain: Domain[_2D] = EuclideanSpace2D

  override def numberOfParameters: Int = productTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): RotationThenTranslation[_2D] = {
    val productTs = productTS.transformationForParameters(p)
    val translation: Translation[_2D] = productTs.outerTransformation
    val rotation: Rotation[_2D] = productTs.innerTransformation
    RotationThenTranslation(rotation, translation)
  }

  /** returns identity transformation) */
  override def identityTransformation: RotationThenTranslation[_2D] = RotationThenTranslation(
    RotationSpace2D(rotationCenter).identityTransformation,
    TranslationSpace2D.identityTransformation
  )
}

case class RotationThenTranslationSpace3D(rotationCenter: Point[_3D])
    extends TransformationSpaceWithDifferentiableTransforms[_3D] {

  private val productTS = ProductTransformationSpace(TranslationSpace3D, RotationSpace3D(rotationCenter))

  override type T[D] = RotationThenTranslation[D]

  override def domain: Domain[_3D] = EuclideanSpace3D

  override def numberOfParameters: Int = productTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): RotationThenTranslation[_3D] = {
    val productTs = productTS.transformationForParameters(p)
    val translation: Translation[_3D] = productTs.outerTransformation
    val rotation: Rotation[_3D] = productTs.innerTransformation
    RotationThenTranslation(rotation, translation)
  }

  /** returns identity transformation) */
  override def identityTransformation: RotationThenTranslation[_3D] = RotationThenTranslation(
    RotationSpace3D(rotationCenter).identityTransformation,
    TranslationSpace3D.identityTransformation
  )
}
