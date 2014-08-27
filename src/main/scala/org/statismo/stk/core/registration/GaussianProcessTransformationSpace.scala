package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector

import scala.NotImplementedError
import breeze.linalg.DenseVector

import org.statismo.stk.core.statisticalmodel.{GaussianProcess, LowRankGaussianProcess}
import org.statismo.stk.core.geometry._

class GaussianProcessTransformationSpace[D <: Dim] private (gp: GaussianProcess[D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = GaussianProcessTransformation[D]

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  def parametersDimensionality = gp.rank

  def inverseTransform(p: ParameterVector) = None

  def transformForParameters(p: ParameterVector) =  GaussianProcessTransformation[D](gp, p)
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[D] => gp.jacobian(p)(x) }
}


class GaussianProcessTransformation[D <: Dim] private(gp : GaussianProcess[D], alpha: ParameterVector) extends Transformation[D] with CanDifferentiate[D] {

  val instance = gp.instance(alpha)

  def apply(x: Point[D]) : Point[D] = {
    val newPointAsVector = instance(x)
    x+newPointAsVector
  }
  def takeDerivative(x: Point[D]) = { throw new NotImplementedError("take derivative of kernel") }
}

object GaussianProcessTransformation {
  def apply[D <: Dim](gp: GaussianProcess[D], alpha : TransformationSpace.ParameterVector) = new GaussianProcessTransformation[D](gp, alpha) 
}


object GaussianProcessTransformationSpace {
  def apply[D <: Dim](gp: GaussianProcess[D]) = new GaussianProcessTransformationSpace[D](gp)
}

