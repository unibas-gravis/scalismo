package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector

import scala.NotImplementedError
import breeze.linalg.DenseVector

import org.statismo.stk.core.statisticalmodel.{GaussianProcess, LowRankGaussianProcess}
import org.statismo.stk.core.geometry._

case class GaussianProcessTransformationSpace1D(gp: GaussianProcess[_1D]) extends TransformationSpace[_1D] with DifferentiableTransforms[_1D] {
  override type T = GaussianProcessTransformation1D

  def parametersDimensionality = gp.rank

  def inverseTransform(p: ParameterVector) = None

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def transformForParameters(p: ParameterVector) = new GaussianProcessTransformation1D(gp, p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_1D] =>
    gp.jacobian(p)(x)
  }

}


// the actual kernel transform
case class GaussianProcessTransformation1D(gp : GaussianProcess[_1D], alpha: ParameterVector) extends Transformation[_1D] with CanDifferentiate[_1D]{

  val instance = gp.instance(alpha)

  def apply(x: Point[_1D]) : Point[_1D] = {
    val newPointAsVector = instance(x)
    Point(x(0) + newPointAsVector(0))
  }
  def takeDerivative(x: Point[_1D]) = { throw new NotImplementedError("take derivative of kernel") }
}


case class GaussianProcessTransformationSpace2D(gp: GaussianProcess[_2D]) extends TransformationSpace[_2D] with DifferentiableTransforms[_2D] {

  override type T = GaussianProcessTransformation2D

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  def parametersDimensionality = gp.rank


  def inverseTransform(p: ParameterVector) = None

  override def transformForParameters(p: ParameterVector) = new GaussianProcessTransformation2D(gp, p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_2D] =>
    gp.jacobian(p)(x)
  }
}

// the actual kernel transform
case class GaussianProcessTransformation2D(gp : GaussianProcess[_2D], alpha: ParameterVector) extends Transformation[_2D] with CanDifferentiate[_2D] {

  val instance = gp.instance(alpha)

  def apply(x: Point[_2D]) : Point[_2D] = {
    val newPointAsVector = instance(x)
    Point(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1))
  }
  def takeDerivative(x: Point[_2D]) = { throw new NotImplementedError("take derivative of kernel") }
}



case class GaussianProcessTransformationSpace3D(gp: GaussianProcess[_3D]) extends TransformationSpace[_3D] with DifferentiableTransforms[_3D] {

  override type T = GaussianProcessTransformation3D

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  def parametersDimensionality = gp.rank


  def inverseTransform(p: ParameterVector) = None

  def transformForParameters(p: ParameterVector) = new GaussianProcessTransformation3D(gp, p)
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[_3D] =>
    gp.jacobian(p)(x)
  }

}

// the actual kernel transform
case class GaussianProcessTransformation3D(gp : GaussianProcess[_3D], alpha: ParameterVector) extends Transformation[_3D] with CanDifferentiate[_3D] {

  val instance = gp.instance(alpha)

  def apply(x: Point[_3D]) : Point[_3D] = {
    val newPointAsVector = instance(x)
    Point(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1), x(2) + newPointAsVector(2))
  }
  def takeDerivative(x: Point[_3D]) = { throw new NotImplementedError("take derivative of kernel") }
}


object GaussianProcessTransformationSpace {

}

