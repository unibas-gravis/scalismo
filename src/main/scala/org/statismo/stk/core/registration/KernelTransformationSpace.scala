package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseMatrix
import scala.NotImplementedError
import breeze.linalg.DenseVector
import org.statismo.stk.core.image.DiscreteImageDomain1D
import breeze.plot._
import org.statismo.stk.core.image.DiscreteImageDomain1D
import org.statismo.stk.core.image.DiscreteImageDomain2D
import org.statismo.stk.core.image.DiscreteScalarImage1D
import org.statismo.stk.core.image.Interpolation
import org.statismo.stk.core.image.Utils
import org.statismo.stk.core.io.ImageIO
import java.io.File
import org.statismo.stk.core.image.DiscreteImageDomain
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.GradientDescentConfiguration
import org.statismo.stk.core.numerics.UniformSampler1D
import org.statismo.stk.core.numerics.UniformSampler2D
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcess
import org.statismo.stk.core.geometry._

// TODO call them GaussianProcessTransformationSpace
case class KernelTransformationSpaceConfiguration[D <: Dim](
  val gp: LowRankGaussianProcess[D],
  val withValueCaching: Boolean = false)
  extends TransformationSpaceConfiguration

case class KernelTransformationSpace1D(configuration: KernelTransformationSpaceConfiguration[OneD]) extends TransformationSpace[OneD] {
  override type T = KernelTransformation1D

  val gp = configuration.gp

  def parametersDimensionality = gp.eigenPairs.size
  def inverseTransform(p: ParameterVector) = None

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def transformForParameters(p: ParameterVector) = new KernelTransformation1D(gp, p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[OneD] =>
    gp.jacobian(p)(x)
  }

}


// the actual kernel transform
case class KernelTransformation1D(gp : LowRankGaussianProcess[OneD], alpha: ParameterVector) extends Transformation[OneD] {

  val instance = gp.instance(alpha)

  def apply(x: Point[OneD]) : Point[OneD] = {
    val newPointAsVector = instance(x)
    Point1D(x(0) + newPointAsVector(0))
  }
  def takeDerivative(x: Point[OneD]) = { throw new NotImplementedError("take derivative of kernel") }
}


case class KernelTransformationSpace2D(configuration: KernelTransformationSpaceConfiguration[TwoD]) extends TransformationSpace[TwoD] {

  override type T = KernelTransformation2D

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)
  val gp = configuration.gp

  def parametersDimensionality = gp.eigenPairs.size

  def inverseTransform(p: ParameterVector) = None

  override def transformForParameters(p: ParameterVector) = new KernelTransformation2D(gp, p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
    gp.jacobian(p)(x)
  }
}

// the actual kernel transform
case class KernelTransformation2D(gp : LowRankGaussianProcess[TwoD], alpha: ParameterVector) extends Transformation[TwoD] {

  val instance = gp.instance(alpha)

  def apply(x: Point[TwoD]) : Point[TwoD] = {
    val newPointAsVector = instance(x)
    Point2D(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1))
  }
  def takeDerivative(x: Point[TwoD]) = { throw new NotImplementedError("take derivative of kernel") }
}



case class KernelTransformationSpace3D(configuration: KernelTransformationSpaceConfiguration[ThreeD]) extends TransformationSpace[ThreeD] {

  override type T = KernelTransformation3D

  def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)
  val gp = configuration.gp

  def parametersDimensionality = gp.eigenPairs.size

  def inverseTransform(p: ParameterVector) = None

  def transformForParameters(p: ParameterVector) = new KernelTransformation3D(gp, p)
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
    gp.jacobian(p)(x)
  }

}

// the actual kernel transform
case class KernelTransformation3D(gp : LowRankGaussianProcess[ThreeD], alpha: ParameterVector) extends Transformation[ThreeD] {

  val instance = gp.instance(alpha)

  def apply(x: Point[ThreeD]) : Point[ThreeD] = {
    val newPointAsVector = instance(x)
    Point3D(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1), x(2) + newPointAsVector(2))
  }
  def takeDerivative(x: Point[ThreeD]) = { throw new NotImplementedError("take derivative of kernel") }
}


object KernelTransformationSpace {

}

