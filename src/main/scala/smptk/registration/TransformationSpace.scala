package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.image.CoordVector

trait TransformationSpace[CV[A] <: CoordVector[A]] extends Function1[ParameterVector, Transformation[CV]] {
  type JacobianImage = Function1[CV[Float], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivative(alpha : ParameterVector) : JacobianImage
}

trait Transformation[CV[A] <: CoordVector[A]] extends (CV[Float] => CV[Float]) {

}
    
object TransformationSpace {
  type ParameterVector = DenseVector[Float]
	

}