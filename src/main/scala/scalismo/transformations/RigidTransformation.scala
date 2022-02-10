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

case class TranslationAfterRotation[D](translation: Translation[D], rotation: Rotation[D])
    extends RigidTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(translation, rotation)

  override def inverse = RotationAfterTranslation(rotation.inverse, translation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f
}

object TranslationAfterRotation2D {
  def apply(translation: Translation[_2D], rotation: Rotation[_2D]): TranslationAfterRotation[_2D] = {
    TranslationAfterRotation(translation, rotation)
  }
}

object TranslationAfterRotation3D {
  def apply(translation: Translation[_3D], rotation: Rotation[_3D]): TranslationAfterRotation[_3D] = {
    TranslationAfterRotation(translation, rotation)
  }
}

case class RotationAfterTranslation[D](rotation: Rotation[D], translation: Translation[D])
    extends RigidTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(rotation, translation)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f

  override def inverse = TranslationAfterRotation(translation.inverse, rotation.inverse)
}

object RotationAfterTranslation2D {
  def apply(rotation: Rotation[_2D], translation: Translation[_2D]): RotationAfterTranslation[_2D] = {
    RotationAfterTranslation(rotation, translation)
  }
}

object RotationAfterTranslation3D {
  def apply(translation: Translation[_3D], rotation: Rotation[_3D]): RotationAfterTranslation[_3D] = {
    RotationAfterTranslation(rotation, translation)
  }
}

case class TranslationAfterRotationSpace2D(rotationCenter: Point[_2D])
    extends TransformationSpaceWithDifferentiableTransforms[_2D] {

  private val productTS = ProductTransformationSpace(TranslationSpace2D, RotationSpace2D(rotationCenter))

  override type T[D] = TranslationAfterRotation[D]

  override def domain: Domain[_2D] = EuclideanSpace2D

  override def numberOfParameters: Int = productTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): TranslationAfterRotation[_2D] = {
    val productTs = productTS.transformationForParameters(p)
    val translation: Translation[_2D] = productTs.outerTransformation
    val rotation: Rotation[_2D] = productTs.innerTransformation
    TranslationAfterRotation(translation, rotation)
  }

  /** returns identity transformation) */
  override def identityTransformation: TranslationAfterRotation[_2D] = TranslationAfterRotation(
    TranslationSpace2D.identityTransformation,
    RotationSpace2D(rotationCenter).identityTransformation
  )
}

case class TranslationAfterRotationSpace3D(rotationCenter: Point[_3D])
    extends TransformationSpaceWithDifferentiableTransforms[_3D] {

  private val productTS = ProductTransformationSpace(TranslationSpace3D, RotationSpace3D(rotationCenter))

  override type T[D] = TranslationAfterRotation[D]

  override def domain: Domain[_3D] = EuclideanSpace3D

  override def numberOfParameters: Int = productTS.numberOfParameters

  override def transformationForParameters(p: ParameterVector): TranslationAfterRotation[_3D] = {
    val productTs = productTS.transformationForParameters(p)
    val translation: Translation[_3D] = productTs.outerTransformation
    val rotation: Rotation[_3D] = productTs.innerTransformation
    TranslationAfterRotation(translation, rotation)
  }

  /** returns identity transformation) */
  override def identityTransformation: TranslationAfterRotation[_3D] = TranslationAfterRotation(
    TranslationSpace3D.identityTransformation,
    RotationSpace3D(rotationCenter).identityTransformation
  )
}
