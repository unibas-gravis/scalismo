package smptk
package registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseMatrix
import scala.NotImplementedError
import breeze.linalg.DenseVector
import smptk.image.DiscreteImageDomain1D
import breeze.plot._
import smptk.image.DiscreteImageDomain1D
import smptk.image.DiscreteImageDomain2D
import smptk.image.Image._
import smptk.image.DiscreteScalarImage1D
import smptk.image.Interpolation
import smptk.image.Utils
import smptk.io.ImageIO
import java.io.File
import smptk.image.DiscreteImageDomain
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.GradientDescentConfiguration
import smptk.numerics.UniformSampler1D
import smptk.numerics.UniformSampler2D
import smptk.numerics.UniformSampler3D
import smptk.numerics.UniformSampler
import smptk.statisticalmodel.LowRankGaussianProcess
import smptk.geometry._

case class KernelTransformationSpaceConfiguration[D <: Dim](
  val gp: LowRankGaussianProcess[D],
  val withValueCaching: Boolean = false)
  extends TransformationSpaceConfiguration

case class KernelTransformationSpace1D(configuration: KernelTransformationSpaceConfiguration[OneD]) extends TransformationSpace[OneD] {

  val gp = configuration.gp
  def parametersDimensionality = gp.eigenPairs.size
  def inverseTransform(p: ParameterVector) = None

  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)

  // the actual kernel transform
  case class KernelTransformation1D(alpha: ParameterVector) extends Transformation[OneD] {

    val instance = configuration.gp.instance(alpha)

    def apply(x: Point[OneD]) : Point[OneD] = {
      val newPointAsVector = instance(x)
      Point1D(x(0) + newPointAsVector(0))
    }
    def takeDerivative(x: Point[OneD]) = { throw new NotImplementedError("take derivative of kernel") }
  }

  def apply(p: ParameterVector) =
    if (configuration.withValueCaching)
      new KernelTransformation1D(p) with ValueCaching[OneD]
    else
      KernelTransformation1D(p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[OneD] =>
    gp.jacobian(p)(x)
  }

}

case class KernelTransformationSpace2D(configuration: KernelTransformationSpaceConfiguration[TwoD]) extends TransformationSpace[TwoD] {

  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp

  def parametersDimensionality = gp.eigenPairs.size

  def inverseTransform(p: ParameterVector) = None

  // the actual kernel transform
  case class KernelTransformation2D(alpha: ParameterVector) extends Transformation[TwoD] {

    val instance = configuration.gp.instance(alpha)

    def apply(x: Point[TwoD]) : Point[TwoD] = {
      val newPointAsVector = instance(x)
      Point2D(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1))
    }
    def takeDerivative(x: Point[TwoD]) = { throw new NotImplementedError("take derivative of kernel") }
  }

  def apply(p: ParameterVector) = if (configuration.withValueCaching)
    new KernelTransformation2D(p) with ValueCaching[TwoD]
  else
    KernelTransformation2D(p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
    gp.jacobian(p)(x)
  }

}

case class KernelTransformationSpace3D(configuration: KernelTransformationSpaceConfiguration[ThreeD]) extends TransformationSpace[ThreeD] {

  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp

  def parametersDimensionality = gp.eigenPairs.size

  def inverseTransform(p: ParameterVector) = None

  // the actual kernel transform
  case class KernelTransformation3D(alpha: ParameterVector) extends Transformation[ThreeD] {

    val instance = configuration.gp.instance(alpha)

    def apply(x: Point[ThreeD]) : Point[ThreeD] = {
      val newPointAsVector = instance(x)
      Point3D(x(0) + newPointAsVector(0), x(1) + newPointAsVector(1), x(2) + newPointAsVector(2))
    }
    def takeDerivative(x: Point[ThreeD]) = { throw new NotImplementedError("take derivative of kernel") }
  }

  def apply(p: ParameterVector) = if (configuration.withValueCaching)
    new KernelTransformation3D(p) with ValueCaching[ThreeD]
  else
    KernelTransformation3D(p)

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
    gp.jacobian(p)(x)
  }

}

object KernelTransformationSpace {

}

