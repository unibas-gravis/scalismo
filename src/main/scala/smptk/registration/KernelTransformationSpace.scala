package smptk.registration

import smptk.image.Geometry._
import TransformationSpace.ParameterVector
import breeze.linalg.DenseMatrix
import scala.NotImplementedError
import breeze.linalg.DenseVector
import smptk.image.CoordVector
import smptk.image.DiscreteImageDomain1D
import breeze.plot._
import smptk.image.DiscreteImageDomain1D
import smptk.image.DiscreteImageDomain2D
import smptk.image.Geometry.implicits._
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

case class KernelTransformationSpaceConfiguration[CV[A] <: CoordVector[A]](
  val gp: LowRankGaussianProcess[CV],
  val withValueCaching: Boolean = false)
  extends TransformationSpaceConfiguration

case class KernelTransformationSpace1D(configuration: KernelTransformationSpaceConfiguration[CoordVector1D]) extends TransformationSpace[CoordVector1D] {

  val gp = configuration.gp  
  def parametersDimensionality = gp.effectiveNumBasisFunctions
  def inverseTransform(p : ParameterVector) = None

  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  

  // the actual kernel transform
  case class KernelTransformation1D(alpha: ParameterVector) extends Transformation[CoordVector1D] {

    val instance = configuration.gp.instance(alpha)

    def apply(x: Point1D) = {
      val newPointAsVector = instance(x)
      CoordVector1D(newPointAsVector(0))
    }
    def takeDerivative(x: Point1D) = { throw new NotImplementedError("take derivative of kernel") }
  }

  def apply(p : ParameterVector) = KernelTransformation1D(p)
  
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point1D =>
  	gp.jacobian(p)(x)
  }

  
}

case class KernelTransformationSpace2D(configuration: KernelTransformationSpaceConfiguration[CoordVector2D]) extends TransformationSpace[CoordVector2D] {

  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp

  def parametersDimensionality = gp.effectiveNumBasisFunctions
  def inverseTransform(p : ParameterVector) = None


  // the actual kernel transform
  case class KernelTransformation2D(alpha: ParameterVector) extends Transformation[CoordVector2D] {

    val instance = configuration.gp.instance(alpha)

    def apply(x: Point2D) = {
      val newPointAsVector = instance(x)
      CoordVector2D(newPointAsVector(0), newPointAsVector(1))
    }
    def takeDerivative(x: Point2D) = { throw new NotImplementedError("take derivative of kernel") }
  }

  
    def apply(p : ParameterVector) = KernelTransformation2D(p)
  
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point2D =>
  	gp.jacobian(p)(x)
  }

}

case class KernelTransformationSpace3D(configuration: KernelTransformationSpaceConfiguration[CoordVector3D]) extends TransformationSpace[CoordVector3D] {

  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp

  def parametersDimensionality = gp.effectiveNumBasisFunctions
  def inverseTransform(p : ParameterVector) = None

  // the actual kernel transform
  case class KernelTransformation3D(alpha: ParameterVector) extends Transformation[CoordVector3D] {

    val instance = configuration.gp.instance(alpha)

    def apply(x: Point3D) = {
      val newPointAsVector = instance(x)
      CoordVector3D(newPointAsVector(0), newPointAsVector(1), newPointAsVector(2))
    }
    def takeDerivative(x: Point3D) = { throw new NotImplementedError("take derivative of kernel") }
  }

    def apply(p : ParameterVector) = KernelTransformation3D(p)
  
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point3D =>
  	gp.jacobian(p)(x)
  }

}

object KernelTransformationSpace {

}

