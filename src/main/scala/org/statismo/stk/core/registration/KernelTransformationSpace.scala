package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector

import scala.NotImplementedError
import breeze.linalg.DenseVector

import org.statismo.stk.core.statisticalmodel.{GaussianProcess, LowRankGaussianProcess}
import org.statismo.stk.core.geometry._

case class GaussianProcessTransformationSpace1D(gp: GaussianProcess[OneD]) extends TransformationSpace[OneD] {
  override type T = GaussianProcessTransformation1D

  def parametersDimensionality = gp.rank

  def inverseTransform(p: ParameterVector) = None

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def transformForParameters(p: ParameterVector) = new GaussianProcessTransformation1D(gp, p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[OneD] =>
    gp.jacobian(p)(x)
  }

}


// the actual kernel transform
case class GaussianProcessTransformation1D(gp : GaussianProcess[OneD], alpha: ParameterVector) extends Transformation[OneD] {

  val instance = gp.instance(alpha)

  def apply(x: Point[OneD]) : Point[OneD] = {
    val newPointAsVector = instance(x)
    Point1D(x(0) + newPointAsVector(0))
  }
  def takeDerivative(x: Point[OneD]) = { throw new NotImplementedError("take derivative of kernel") }
}


case class GaussianProcessTransformationSpace2D(gp: GaussianProcess[TwoD]) extends TransformationSpace[TwoD] {

  override type T = GaussianProcessTransformation2D

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  def parametersDimensionality = gp.rank


  def inverseTransform(p: ParameterVector) = None

  override def transformForParameters(p: ParameterVector) = new GaussianProcessTransformation2D(gp, p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
    gp.jacobian(p)(x)
  }
}

// the actual kernel transform
case class GaussianProcessTransformation2D(gp : GaussianProcess[TwoD], alpha: ParameterVector) extends Transformation[TwoD] {

  val instance = gp.instance(alpha)

  def apply(x: Point[TwoD]) : Point[TwoD] = {
    val newPointAsVector = instance(x)
    Point2D(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1))
  }
  def takeDerivative(x: Point[TwoD]) = { throw new NotImplementedError("take derivative of kernel") }
}



case class GaussianProcessTransformationSpace3D(gp: GaussianProcess[ThreeD]) extends TransformationSpace[ThreeD] {

  override type T = GaussianProcessTransformation3D

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  def parametersDimensionality = gp.rank


  def inverseTransform(p: ParameterVector) = None

  def transformForParameters(p: ParameterVector) = new GaussianProcessTransformation3D(gp, p)
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
    gp.jacobian(p)(x)
  }

}

// the actual kernel transform
case class GaussianProcessTransformation3D(gp : GaussianProcess[ThreeD], alpha: ParameterVector) extends Transformation[ThreeD] {

  val instance = gp.instance(alpha)

  def apply(x: Point[ThreeD]) : Point[ThreeD] = {
    val newPointAsVector = instance(x)
    Point3D(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1), x(2) + newPointAsVector(2))
  }
  def takeDerivative(x: Point[ThreeD]) = { throw new NotImplementedError("take derivative of kernel") }
}


object GaussianProcessTransformationSpace {

}

