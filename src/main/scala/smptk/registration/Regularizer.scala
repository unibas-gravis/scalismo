package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector

trait Regularizer extends (ParameterVector => Float) { 
  
  def takeDerivative(p : ParameterVector) : DenseVector[Float]
}

object RKHSNormRegularizer extends Regularizer {
	def apply(alpha : ParameterVector) = alpha.norm(2).toFloat
  
  def takeDerivative(alpha : ParameterVector) = alpha
}