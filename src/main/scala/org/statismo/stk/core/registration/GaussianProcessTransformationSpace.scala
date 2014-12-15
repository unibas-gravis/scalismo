package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector

import breeze.linalg.DenseVector
import org.statismo.stk.core.statisticalmodel.{ GaussianProcess, LowRankGaussianProcess }
import org.statismo.stk.core.geometry._

class GaussianProcessTransformationSpace[D <: Dim] private (gp: GaussianProcess[D]) extends TransformationSpace[D] {

  override type T = GaussianProcessTransformation[D]

  override def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def parametersDimensionality = gp.rank

  override def transformForParameters(p: ParameterVector) = GaussianProcessTransformation[D](gp, p)
  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[D] => gp.jacobian(p)(x)
  }
}

class GaussianProcessTransformation[D <: Dim] private (gp: GaussianProcess[D], alpha: ParameterVector) extends ParametricTransformation[D] {

  val instance = gp.instance(alpha)
  val parameters = alpha
  override def apply(x: Point[D]): Point[D] = {
    val newPointAsVector = instance(x)
    x + newPointAsVector
  }
}

object GaussianProcessTransformation {
  def apply[D <: Dim](gp: GaussianProcess[D], alpha: TransformationSpace.ParameterVector) = new GaussianProcessTransformation[D](gp, alpha)
}

object GaussianProcessTransformationSpace {
  def apply[D <: Dim](gp: GaussianProcess[D]) = new GaussianProcessTransformationSpace[D](gp)
}

