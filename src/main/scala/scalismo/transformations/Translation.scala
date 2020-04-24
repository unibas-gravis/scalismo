package scalismo.transformations

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{EuclideanSpace, Field, RealSpace}
import scalismo.geometry.{_1D, _2D, _3D, EuclideanVector, NDSpace, Point, SquareMatrix}
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.transformations.{TransformationSpace}
import scalismo.transformations.TransformationSpace.{ParameterVector}

/**
 * D-dimensional translation transform that is parametric, invertible and differentiable
 *
 *  @param t Translation vector
 */
case class Translation[D: NDSpace](t: EuclideanVector[D])
    extends ParametricTransformation[D]
    with CanInvert[D]
    with CanDifferentiate[D] {

  override val f = (pt: Point[D]) => pt + t

  override val domain = EuclideanSpace[D]

  override val numberOfParameters: Int = NDSpace[D].dimensionality

  override def derivative: Point[D] => SquareMatrix[D] = (_: Point[D]) => SquareMatrix.eye[D]

  override def inverse: Translation[D] = new Translation(t * (-1f))

  /**parameters are the coordinates of the translation vector*/
  val parameters = t.toBreezeVector

  override def jacobian: JacobianField[D] = {
    Field(domain, (x: Point[D]) => DenseMatrix.eye[Double](numberOfParameters))

  }
}

object Translation1D {
  def apply(t: EuclideanVector[_1D]): Translation[_1D] = {
    new Translation[_1D](t)
  }
}

object Translation2D {
  def apply(t: EuclideanVector[_2D]): Translation[_2D] = {
    new Translation[_2D](t)
  }
}

object Translation3D {
  def apply(t: EuclideanVector[_3D]): Translation[_3D] = {
    new Translation[_3D](t)
  }
}

/**
 * Parametric transformations space producing translation transforms
 */
case class TranslationSpace[D: NDSpace]() extends TransformationSpaceWithDifferentiableTransforms[D] {

  override type T[D] = Translation[D]
  override val domain = EuclideanSpace[D]

  def numberOfParameters: Int = implicitly[NDSpace[D]].dimensionality
  override def identityTransformation = {
    transformationForParameters(DenseVector.zeros(numberOfParameters))
  }

  /**Returns a translation transform, where the translation vectors' coordinates are the given parameters*/
  override def transformationForParameters(p: ParameterVector): Translation[D] =
    Translation[D](EuclideanVector[D](p.data))

}

object TranslationSpace1D {
  def apply(): TranslationSpace[_1D] = TranslationSpace[_1D]()
}

object TranslationSpace2D {
  def apply(): TranslationSpace[_2D] = TranslationSpace[_2D]()
}

object TranslationSpace3D {
  def apply(): TranslationSpace[_3D] = TranslationSpace[_3D]()
}
