package scalismo.transformations

import breeze.linalg.DenseVector
import scalismo.common.{Domain, EuclideanSpace, EuclideanSpace2D, EuclideanSpace3D}
import scalismo.geometry.{_1D, _2D, _3D, EuclideanVector, EuclideanVector3D, NDSpace, Point, SquareMatrix}
import scalismo.transformations
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.TransformationSpace.ParameterVector

trait SimilarityTransformation[D]
    extends ParametricTransformation[D]
    with CanDifferentiateWRTPosition[D]
    with CanInvert[D, SimilarityTransformation]

case class ScalingAfterRotation[D](scaling: Scaling[D], rotation: Rotation[D]) extends SimilarityTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(scaling, rotation)

  override def inverse = new RotationAfterScaling(rotation.inverse, scaling.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f
}

object ScalingAfterRotation2D {
  def apply(scaling: Scaling[_2D], rotation: Rotation[_2D]): ScalingAfterRotation[_2D] = {
    ScalingAfterRotation(scaling, rotation)
  }
}

object ScalingAfterRotation3D {
  def apply(scaling: Scaling[_3D], rotation: Rotation[_3D]): ScalingAfterRotation[_3D] = {
    ScalingAfterRotation(scaling, rotation)
  }
}

case class RotationAfterScaling[D](rotation: Rotation[D], scaling: Scaling[D]) extends SimilarityTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(rotation, scaling)

  override def inverse = ScalingAfterRotation[D](scaling.inverse, rotation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f
}

object RotationAfterScaling2D {
  def apply(scaling: Scaling[_2D], rotation: Rotation[_2D]): RotationAfterScaling[_2D] = {
    RotationAfterScaling(rotation, scaling)
  }
}

object RotationAfterScaling3D {
  def apply(rotation: Rotation[_3D], scaling: Scaling[_3D]): RotationAfterScaling[_3D] = {
    RotationAfterScaling(rotation, scaling)
  }
}

case class TranslationAfterScalingAfterRotation[D](translation: Translation[D],
                                                   scaling: Scaling[D],
                                                   rotation: Rotation[D]
) extends SimilarityTransformation[D] {

  val compTrans = CompositeDifferentiableTransformation(translation, ScalingAfterRotation(scaling, rotation))

  override def inverse = RotationAfterScalingAfterTranslation[D](rotation.inverse, scaling.inverse, translation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f

}
object TranslationAfterScalingAfterRotation2D {
  def apply(translation: Translation[_2D],
            scaling: Scaling[_2D],
            rotation: Rotation[_2D]
  ): TranslationAfterScalingAfterRotation[_2D] = {
    TranslationAfterScalingAfterRotation(translation, scaling, rotation)
  }
}

object TranslationAfterScalingAfterRotation3D {
  def apply(translation: Translation[_3D],
            scaling: Scaling[_3D],
            rotation: Rotation[_3D]
  ): TranslationAfterScalingAfterRotation[_3D] = {
    TranslationAfterScalingAfterRotation(translation, scaling, rotation)
  }
}

case class RotationAfterScalingAfterTranslation[D](rotation: Rotation[D],
                                                   scaling: Scaling[D],
                                                   translation: Translation[D]
) extends SimilarityTransformation[D] {

  val compTrans = CompositeDifferentiableTransformation(RotationAfterScaling(rotation, scaling), translation)

  override def inverse = TranslationAfterScalingAfterRotation[D](translation.inverse, scaling.inverse, rotation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f

}

object RotationAfterScalingAfterTranslation2D {
  def apply(rotation: Rotation[_2D],
            scaling: Scaling[_2D],
            translation: Translation[_2D]
  ): RotationAfterScalingAfterTranslation[_2D] = {
    RotationAfterScalingAfterTranslation(rotation, scaling, translation)
  }
}

object RotationAfterScalingAfterTranslation3D {
  def apply(rotation: Rotation[_3D],
            scaling: Scaling[_3D],
            translation: Translation[_3D]
  ): RotationAfterScalingAfterTranslation[_3D] = {
    RotationAfterScalingAfterTranslation(rotation, scaling, translation)
  }
}

case class TranslationAfterScalingAfterRotationSpace2D(rotationCenter: Point[_2D])
    extends TransformationSpaceWithDifferentiableTransforms[_2D] {

  override type T[D] = TranslationAfterScalingAfterRotation[D]
  private val rotationSpace = RotationSpace2D(rotationCenter)
  private val translationSpace = TranslationSpace2D
  private val scalingSpace = ScalingSpace2D

  override def domain: Domain[_2D] = EuclideanSpace2D

  override def numberOfParameters: Int =
    rotationSpace.numberOfParameters + translationSpace.numberOfParameters + scalingSpace.numberOfParameters

  override def transformationForParameters(p: ParameterVector): TranslationAfterScalingAfterRotation[_2D] = {

    val translation = translationSpace.transformationForParameters(p(0 until 3))
    val scaling = scalingSpace.transformationForParameters(p(3 until 4))
    val rotation = rotationSpace.transformationForParameters(p(4 until 7))
    TranslationAfterScalingAfterRotation(translation, scaling, rotation)
  }

  /** returns identity transformation) */
  override def identityTransformation: TranslationAfterScalingAfterRotation[_2D] = TranslationAfterScalingAfterRotation(
    translationSpace.identityTransformation,
    scalingSpace.identityTransformation,
    rotationSpace.identityTransformation
  )
}

case class TranslationAfterScalingAfterRotationSpace3D(rotationCenter: Point[_3D])
    extends TransformationSpaceWithDifferentiableTransforms[_3D] {

  override type T[D] = TranslationAfterScalingAfterRotation[D]
  private val rotationSpace = RotationSpace3D(rotationCenter)
  private val translationSpace = TranslationSpace3D
  private val scalingSpace = ScalingSpace3D

  override def domain: Domain[_3D] = EuclideanSpace3D

  override def numberOfParameters: Int =
    rotationSpace.numberOfParameters + translationSpace.numberOfParameters + scalingSpace.numberOfParameters

  override def transformationForParameters(p: ParameterVector): TranslationAfterScalingAfterRotation[_3D] = {

    val translation = translationSpace.transformationForParameters(p(0 until 3))
    val scaling = scalingSpace.transformationForParameters(p(3 until 4))
    val rotation = rotationSpace.transformationForParameters(p(4 until 7))
    TranslationAfterScalingAfterRotation(translation, scaling, rotation)
  }

  /** returns identity transformation) */
  override def identityTransformation: TranslationAfterScalingAfterRotation[_3D] = TranslationAfterScalingAfterRotation(
    translationSpace.identityTransformation,
    scalingSpace.identityTransformation,
    rotationSpace.identityTransformation
  )
}
