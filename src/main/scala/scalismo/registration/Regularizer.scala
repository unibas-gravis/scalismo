package scalismo.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector

trait Regularizer extends (ParameterVector => Double) {
  
  def takeDerivative(p : ParameterVector) : DenseVector[Float]
}

object RKHSNormRegularizer extends Regularizer {
	def apply(alpha : ParameterVector) = { val t = breeze.linalg.norm(alpha, 2);  t*t}
  
  def takeDerivative(alpha : ParameterVector) = alpha * 2f

}