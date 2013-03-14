package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector

trait Regularizer extends (ParameterVector => Double) { 
  
  def takeDerivative(p : ParameterVector) : DenseVector[Double]
}

object RKHSNormRegularizer extends Regularizer {
	def apply(alpha : ParameterVector) = alpha.norm(2)
  
  def takeDerivative(alpha : ParameterVector) = alpha
}