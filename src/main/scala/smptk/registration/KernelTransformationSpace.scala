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


case class KernelTransformationSpaceConfiguration[CV[A] <: CoordVector[A]](
  val numComponents: Int,
  val numPointsForNystrom: Int,
  val gp: GaussianProcess[CV],
  val withValueCaching: Boolean = false)

  extends TransformationSpaceConfiguration


case class KernelTransformationSpace1D(configuration: KernelTransformationSpaceConfiguration[CoordVector1D]) extends TransformationSpace[CoordVector1D] {

  def parametersDimensionality = configuration.numComponents
  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp
  
  val sampler = UniformSampler1D(configuration.numPointsForNystrom)
  val (eigenPairs, effectiveNumParameters) = Kernel.computeNystromApproximation(gp.cov, gp.domain, configuration.numComponents,sampler)

  def apply(p: ParameterVector) = {
    if (configuration.withValueCaching)
      new KernelTransformation1D(p) with ValueCaching[CoordVector1D]
    else
      KernelTransformation1D(p)
  }
  def inverseTransform(p: ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point1D =>
    val J = DenseMatrix.zeros[Double](1, effectiveNumParameters)
    for (((lambda, phi), j) <- eigenPairs.zipWithIndex) (J(0, j) = math.sqrt(lambda) * phi(x)(0))
    J

  }

  // the actual kernel transform
  case class KernelTransformation1D(alpha: ParameterVector) extends Transformation[CoordVector1D] {
    require(alpha.size == eigenPairs.size)

    def apply(x: Point1D) = {

      val defValue = eigenPairs.zipWithIndex.par.map(eigPairWithIndex => {
        val ((lambda, phi), i) = eigPairWithIndex
        phi(x)(0) * alpha(i) * math.sqrt(lambda)
      }).foldLeft(0.)(_ + _)

      CoordVector1D(x(0) + defValue + gp.mean(x)(0))
    }

    def takeDerivative(x: Point1D) = { throw new NotImplementedError("take derivative of kernel") }
  }

}

case class KernelTransformationSpace2D(configuration: KernelTransformationSpaceConfiguration[CoordVector2D]) extends TransformationSpace[CoordVector2D] {

  def parametersDimensionality = configuration.numComponents
  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp
  val domain = gp.domain
  val sampler = UniformSampler2D(configuration.numPointsForNystrom)
  val (eigenPairs, effectiveNumParameters) = Kernel.computeNystromApproximation(gp.cov, gp.domain, configuration.numComponents, sampler)

  def apply(p: ParameterVector) = {
    if (configuration.withValueCaching){
      new KernelTransformation2D(p) with ValueCaching[CoordVector2D]
    }
    else
      KernelTransformation2D(p)
  }

  def inverseTransform(p: ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point2D =>
    val J = DenseMatrix.zeros[Double](2, effectiveNumParameters)
    for (((lambda, phi), j) <- eigenPairs.zipWithIndex) {
      J(::, j) := phi(x) * math.sqrt(lambda)
    }
    J
  }

  // the actual kernel transform
  case class KernelTransformation2D(alpha: ParameterVector) extends Transformation[CoordVector2D] {
    require(alpha.size == eigenPairs.size)

    def apply(x: CoordVector2D[Double]) = {
      val zero = DenseVector(0., 0.)
      //      val defValue = eigenPairs.zipWithIndex.foldLeft(zero)((sum , eigPairWithIndex) => {
      //        val ((lambda, phi), i) = eigPairWithIndex
      //        sum + phi(x) * alpha(i) * math.sqrt(lambda)
      //      })
      val defValue = eigenPairs.zipWithIndex.par.map(eigPairWithIndex => {
        val ((lambda, phi), i) = eigPairWithIndex
        phi(x) * alpha(i) * math.sqrt(lambda)
      }).foldLeft(zero)(_ + _)

      CoordVector2D(x(0) + defValue(0) + gp.mean(x)(0), x(1) + defValue(1) + gp.mean(x)(1))
    }

    def takeDerivative(x: Point2D) = { throw new NotImplementedError("take derivative of kernel") }
  }

}

case class KernelTransformationSpace3D(configuration: KernelTransformationSpaceConfiguration[CoordVector3D]) extends TransformationSpace[CoordVector3D] {

  def parametersDimensionality = configuration.numComponents
  def identityTransformParameters = DenseVector.zeros[Double](parametersDimensionality)
  val gp = configuration.gp
  val domain = gp.domain
  val sampler = UniformSampler3D(configuration.numPointsForNystrom)
  val (eigenPairs, effectiveNumParameters) = Kernel.computeNystromApproximation(gp.cov, gp.domain, configuration.numComponents, sampler)

  def apply(p: ParameterVector) = {
    if (configuration.withValueCaching){
      new KernelTransformation3D(p) with ValueCaching[CoordVector3D]
    }
    else
      KernelTransformation3D(p)
  }

  def inverseTransform(p: ParameterVector) = None
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point3D =>
    val J = DenseMatrix.zeros[Double](3, effectiveNumParameters)
    for (((lambda, phi), j) <- eigenPairs.zipWithIndex) {
      J(::, j) := phi(x) * math.sqrt(lambda)
    }
    J
  }

  // the actual kernel transform
  case class KernelTransformation3D(alpha: ParameterVector) extends Transformation[CoordVector3D] {
    require(alpha.size == eigenPairs.size)

    def apply(x: CoordVector3D[Double]) = {
      val zero = DenseVector(0., 0., 0.)
      //      val defValue = eigenPairs.zipWithIndex.foldLeft(zero)((sum , eigPairWithIndex) => {
      //        val ((lambda, phi), i) = eigPairWithIndex
      //        sum + phi(x) * alpha(i) * math.sqrt(lambda)
      //      })
      val defValue = eigenPairs.zipWithIndex.par.map(eigPairWithIndex => {
        val ((lambda, phi), i) = eigPairWithIndex
        phi(x) * alpha(i) * math.sqrt(lambda)
      }).foldLeft(zero)(_ + _)

      CoordVector3D(x(0) + defValue(0) + gp.mean(x)(0), x(1) + defValue(1) + gp.mean(x)(1), x(2) + defValue(2) + gp.mean(x)(2))
    }

    def takeDerivative(x: Point3D) = { throw new NotImplementedError("take derivative of kernel") }
  }

}

object KernelTransformationSpace {

}

