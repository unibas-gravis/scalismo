package smptk.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector

trait Regularizer extends (ParameterVector => Double) { 
  
  def takeDerivative(p : ParameterVector) : DenseVector[Float]
}

object RKHSNormRegularizer extends Regularizer {
	def apply(alpha : ParameterVector) = alpha.norm(2)
  
  def takeDerivative(alpha : ParameterVector) = alpha
}