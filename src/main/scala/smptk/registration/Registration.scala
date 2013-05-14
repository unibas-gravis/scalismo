
package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.image.ContinuousScalarImage
import smptk.image.ContinuousVectorImage
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.LBFGSOptimizer
import smptk.numerics.CostFunction
import smptk.image.CoordVector
import smptk.common.DiscreteDomain
import smptk.image.DiscreteImageDomain
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry._
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage2D
import smptk.image.ContinuousScalarImage3D
import smptk.image.DiscreteImageDomain2D
import smptk.numerics.Optimizer
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.Sampler
import smptk.numerics.SampleOnceSampler
import smptk.image.DiscreteImageDomain3D
import smptk.image.Utils
import smptk.image.ContinuousScalarImage3D
import smptk.image.DiscreteImage3D
import smptk.image.DiscreteScalarImage3D
import smptk.image.DiscreteImageDomain3D
import smptk.common.BoxedRegion3D

case class RegistrationResult[CV[A] <: CoordVector[A]](transform: Transformation[CV], parameters: ParameterVector) {}

case class RegistrationConfiguration[CV[A] <: CoordVector[A]](
  val optimizer: Optimizer,
  val integrator: Integrator[CV],
  val metric: ImageMetric[CV],
  val transformationSpace: TransformationSpace[CV],
  val regularizer: Regularizer,
  val regularizationWeight: Double,
  initialParametersOrNone: Option[DenseVector[Double]] = None) {
  def initialParameters = initialParametersOrNone.getOrElse(transformationSpace.identityTransformParameters)
}

object Registration {

  def registrationND[CV[A] <: CoordVector[A]](configuration: RegistrationConfiguration[CV])(
    fixedImage: ContinuousScalarImage[CV],
    movingImage: ContinuousScalarImage[CV]): (DiscreteImageDomain[CV] => RegistrationResult[CV]) =
    {
      fixedImageRegion =>
        {
          val regularizer = RKHSNormRegularizer

          val transformationSpace = configuration.transformationSpace
          
          val costFunction = new CostFunction {
            def onlyValue(params: ParameterVector): Double = {
              val transformation = transformationSpace(params)
              val warpedImage = movingImage.backwardWarp(transformation, fixedImage.isDefinedAt)

              configuration.metric(warpedImage, fixedImage)(configuration.integrator, fixedImageRegion) + configuration.regularizationWeight * regularizer(params)

            }
            def apply(params: ParameterVector): (Double, DenseVector[Double]) = {

              // create a new sampler, that simply caches the points and returns the same points in every call
              // this means, we are always using the same samples for computing the integral over the values
              // and the gradient
             
              val sampleStrategy = new SampleOnceSampler(configuration.integrator.sampler)
              val integrationStrategy =Integrator[CV](IntegratorConfiguration(sampleStrategy, configuration.integrator.configuration.numberOfPoints)) 
              
              // compute the value of the cost function
              val transformation = transformationSpace(params)
              val warpedImage = movingImage.backwardWarp(transformation, fixedImage.isDefinedAt)
                           
              val errorVal = configuration.metric(warpedImage, fixedImage)(integrationStrategy,fixedImageRegion)
              val value = errorVal + configuration.regularizationWeight * regularizer(params)

              // compute the derivative of the cost function

              val dMetricDalpha = configuration.metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)
              val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)
              //TODO add reg val dRegularizationParam : DenseVector[Float] = regularization.differentiate              

              val movingGradientImage = movingImage.differentiate.get // TODO do proper error handling when image is not differentiable  
              val parametricTransformGradientImage = new ContinuousVectorImage[CV] {
                val pixelDimensionality = params.size
                def isDefinedAt(x: CV[Double]) = warpedImage.isDefinedAt(x) && dMetricDalpha.isDefinedAt(x)
                val f = (x: CV[Double]) => dTransformSpaceDAlpha(x).t * movingGradientImage(transformation(x)) * dMetricDalpha(x)
              }

              val gradient = integrationStrategy.integrateVector(parametricTransformGradientImage, fixedImageRegion)
             
              val dR = regularizer.takeDerivative(params)

              (value, gradient + dR * configuration.regularizationWeight)
            }
          }

          val optimizer = configuration.optimizer

          val optimalParameters = optimizer(configuration.initialParameters, costFunction)
          RegistrationResult(transformationSpace(optimalParameters), optimalParameters)

        }
    }

  def registration1D(configuration: RegistrationConfiguration[CoordVector1D])(
    fixedImage: ContinuousScalarImage1D,
    movingImage: ContinuousScalarImage1D): (DiscreteImageDomain1D => RegistrationResult[CoordVector1D]) =
    {
      registrationND(configuration)(fixedImage, movingImage)
    }

  def registration2D(configuration: RegistrationConfiguration[CoordVector2D])(
    fixedImage: ContinuousScalarImage2D,
    movingImage: ContinuousScalarImage2D): (DiscreteImageDomain2D => RegistrationResult[CoordVector2D]) =
    {
      registrationND(configuration)(fixedImage, movingImage)
    }
  
  def registration3D(configuration: RegistrationConfiguration[CoordVector3D])(
    fixedImage: ContinuousScalarImage3D,
    movingImage: ContinuousScalarImage3D): (DiscreteImageDomain3D => RegistrationResult[CoordVector3D]) =
    {
      registrationND(configuration)(fixedImage, movingImage)
    }
}