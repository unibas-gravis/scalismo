package org.statismo.stk.core
package registration

import scala.NotImplementedError
import TransformationSpace.ParameterVector
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.statisticalmodel.GaussianProcess
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcess

import breeze.linalg.DenseVector


class GaussianProcessTransformationSpace[D <: Dim] private (gp: LowRankGaussianProcess[D, D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = GaussianProcessTransformation[D]

  override def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def parametersDimensionality = gp.rank

  override def transformForParameters(p: ParameterVector) = GaussianProcessTransformation[D](gp, p)
  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[D] => gp.jacobian(p)(x)
  }
}

class GaussianProcessTransformation[D <: Dim] private (gp: LowRankGaussianProcess[D, D], alpha: ParameterVector) extends ParametricTransformation[D] with CanDifferentiate[D] {

  val instance = gp.instance(alpha)
  val parameters = alpha
  override def apply(x: Point[D]): Point[D] = {
    val newPointAsVector = instance(x)
    x + newPointAsVector
  }
  def takeDerivative(x: Point[D]) = { throw new NotImplementedError("take derivative of kernel") }
}

object GaussianProcessTransformation {
  def apply[D <: Dim](gp: LowRankGaussianProcess[D, D], alpha: TransformationSpace.ParameterVector) = new GaussianProcessTransformation[D](gp, alpha)
}

object GaussianProcessTransformationSpace {
  def apply[D <: Dim](gp: LowRankGaussianProcess[D, D]) = new GaussianProcessTransformationSpace[D](gp)
}





