package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import image._
import smptk.image.Geometry._

trait TransformationSpace[CV[A] <: CoordVector[A]] extends Function1[ParameterVector, Transformation[CV]] {
  type JacobianImage = Function1[CV[Float], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivative(alpha: ParameterVector): JacobianImage
}

trait Transformation[CV[A] <: CoordVector[A]] extends (CV[Float] => CV[Float]) {

}

case class TranslationSpace1D extends TransformationSpace[CoordVector1D] {
  def apply(p: ParameterVector) = {
    new Transformation[CoordVector1D] { def apply(pt: Point1D) = p(0) + pt(0) }
  }
  def parametersDimensionality: Int = 1
  def takeDerivative(p: ParameterVector) = { x: Point1D =>
    val M = DenseMatrix.zeros[Float](1, 1)
    M(0, 0) = x(0)
    M
  }
}

case class TranslationSpace2D extends TransformationSpace[CoordVector2D] {

   def parametersDimensionality: Int = 2
    
  def apply(p: ParameterVector) = {
    new Transformation[CoordVector2D] { def apply(pt: Point2D) = CoordVector2D(p(0) + pt(0), p(1) + pt(1)) }
  }

  def takeDerivative(p: ParameterVector) = { x: Point2D =>
    val M = DenseMatrix.zeros[Float](2, 2)
    M(0, 0) = x(0)
    M(1, 1) = x(1)
    M
  }
}


object TransformationSpace {
  type ParameterVector = DenseVector[Float]

}