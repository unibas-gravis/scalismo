package scalismo.transformations

import breeze.linalg.DenseVector
import scalismo.common.{Domain, EuclideanSpace}
import scalismo.geometry.{_1D, _2D, _3D, NDSpace, Point, SquareMatrix}
import scalismo.transformations
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.TransformationSpace.ParameterVector

trait SimilarityTransformation[D]
    extends ParametricTransformation[D]
    with CanDifferentiateWRTPosition[D]
    with CanInvert[D, SimilarityTransformation]

case class RotationThenScaling[D](rotation: Rotation[D], scaling: Scaling[D]) extends SimilarityTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(scaling, rotation)

  override def inverse = new ScalingThenRotation(scaling.inverse, rotation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f
}

object RotationThenScaling2D {
  def apply(rotation: Rotation[_2D], scaling: Scaling[_2D]): RotationThenScaling[_2D] = {
    RotationThenScaling(rotation, scaling)
  }
}

object RotationThenScaling3D {
  def apply(rotation: Rotation[_3D], scaling: Scaling[_3D]): RotationThenScaling[_3D] = {
    RotationThenScaling(rotation, scaling)
  }
}

case class ScalingThenRotation[D](scaling: Scaling[D], rotation: Rotation[D]) extends SimilarityTransformation[D] {

  private val compTrans = CompositeDifferentiableTransformation(rotation, scaling)

  override def inverse = RotationThenScaling[D](rotation.inverse, scaling.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f
}

object ScalingThenRotation2D {
  def apply(scaling: Scaling[_2D], rotation: Rotation[_2D]): ScalingThenRotation[_2D] = {
    ScalingThenRotation(scaling, rotation)
  }
}

object ScalingThenRotation3D {
  def apply(scaling: Scaling[_3D], rotation: Rotation[_3D]): ScalingThenRotation[_3D] = {
    ScalingThenRotation(scaling, rotation)
  }
}

case class RotationThenScalingThenTranslation[D](rotation: Rotation[D],
                                                 scaling: Scaling[D],
                                                 translation: Translation[D])
    extends SimilarityTransformation[D] {

  val compTrans = CompositeDifferentiableTransformation(translation, RotationThenScaling(rotation, scaling))

  override def inverse = TranslationThenScalingThenRotation[D](translation.inverse, scaling.inverse, rotation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f

}
object RotationThenScalingThenTranslation2D {
  def apply(rotation: Rotation[_2D],
            scaling: Scaling[_2D],
            translation: Translation[_2D]): RotationThenScalingThenTranslation[_2D] = {
    RotationThenScalingThenTranslation(rotation, scaling, translation)
  }
}

object RotationThenScalingThenTranslation3D {
  def apply(translation: Translation[_3D],
            scaling: Scaling[_3D],
            rotation: Rotation[_3D]): RotationThenScalingThenTranslation[_3D] = {
    RotationThenScalingThenTranslation(rotation, scaling, translation)
  }
}

case class TranslationThenScalingThenRotation[D](translation: Translation[D],
                                                 scaling: Scaling[D],
                                                 rotation: Rotation[D])
    extends SimilarityTransformation[D] {

  val compTrans = CompositeDifferentiableTransformation(ScalingThenRotation(scaling, rotation), translation)

  override def inverse = RotationThenScalingThenTranslation[D](rotation.inverse, scaling.inverse, translation.inverse)

  override def derivativeWRTPosition: Point[D] => SquareMatrix[D] = compTrans.derivativeWRTPosition

  override def parameters: DenseVector[Double] = compTrans.parameters

  override def numberOfParameters: Int = compTrans.numberOfParameters

  override def derivativeWRTParameters: JacobianField[D] = compTrans.derivativeWRTParameters

  override def domain: Domain[D] = rotation.domain

  override def f: Point[D] => Point[D] = compTrans.f

}

object TranslationThenScalingThenRotation2D {
  def apply(translation: Translation[_2D],
            scaling: Scaling[_2D],
            rotation: Rotation[_2D]): TranslationThenScalingThenRotation[_2D] = {
    TranslationThenScalingThenRotation(translation, scaling, rotation)
  }
}

object TranslationThenScalingThenRotation3D {
  def apply(translation: Translation[_3D],
            scaling: Scaling[_3D],
            rotation: Rotation[_3D]): TranslationThenScalingThenRotation[_3D] = {
    TranslationThenScalingThenRotation(translation, scaling, rotation)
  }
}
