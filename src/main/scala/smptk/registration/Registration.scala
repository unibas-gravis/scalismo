
package smptk.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.image.ContinuousScalarImage
import smptk.image.ContinuousVectorImage
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.LBFGSOptimizer
import smptk.numerics.CostFunction
import smptk.common.DiscreteDomain
import smptk.image.DiscreteImageDomain
import smptk.image.ContinuousScalarImage1D
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage2D
import smptk.image.ContinuousScalarImage3D
import smptk.image.DiscreteImageDomain2D
import smptk.numerics.Optimizer
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.{Sampler, SampleOnceSampler}
import smptk.image.DiscreteImageDomain3D
import smptk.image.Utils
import smptk.image.ContinuousScalarImage3D
import smptk.image.DiscreteImage3D
import smptk.image.DiscreteScalarImage3D
import smptk.image.DiscreteImageDomain3D
import smptk.common.BoxedDomain3D
import smptk.common.BoxedDomain
import smptk.common.BoxedDomain1D
import smptk.common.BoxedDomain2D
import smptk.geometry._

case class RegistrationResult[D <: Dim](transform: Transformation[D], parameters: ParameterVector, configurationFileName : Option[String] = None) {}

case class RegistrationConfiguration[D <: Dim](
  val optimizer: Optimizer,
  val integrator: Integrator[D],
  val metric: ImageMetric[D],
  val transformationSpace: TransformationSpace[D],
  val regularizer: Regularizer,
  val regularizationWeight: Double,
  initialParametersOrNone: Option[DenseVector[Float]] = None) {
  def initialParameters = initialParametersOrNone.getOrElse(transformationSpace.identityTransformParameters)
}

object Registration {

  def registrationND[D <: Dim : DimTraits](configuration: RegistrationConfiguration[D])(
    fixedImage: ContinuousScalarImage[D],
    movingImage: ContinuousScalarImage[D]): (BoxedDomain[D] => RegistrationResult[D]) =
    {
      fixedImageRegion =>
        {
          val regularizer = RKHSNormRegularizer

          val transformationSpace = configuration.transformationSpace
          
          val costFunction = new CostFunction {
            def onlyValue(params: ParameterVector): Double = {
              val transformation = transformationSpace(params)
              val warpedImage = movingImage.compose(transformation)

              configuration.metric(warpedImage, fixedImage)(configuration.integrator) + configuration.regularizationWeight * regularizer(params)

            }
            def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

              // create a new sampler, that simply caches the points and returns the same points in every call
              // this means, we are always using the same samples for computing the integral over the values
              // and the gradient
             
              val sampleStrategy = new SampleOnceSampler(configuration.integrator.sampler)
              val integrationStrategy =Integrator[D](IntegratorConfiguration(sampleStrategy, configuration.integrator.configuration.numberOfPoints)) 
              
              // compute the value of the cost function
              val transformation = transformationSpace(params)
              val warpedImage = movingImage.compose(transformation)
                           
              val errorVal = configuration.metric(warpedImage, fixedImage)(integrationStrategy)
              val value = errorVal + configuration.regularizationWeight * regularizer(params)

              // compute the derivative of the cost function

              val dMetricDalpha = configuration.metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)
              val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)
              //TODO add reg val dRegularizationParam : DenseVector[Float] = regularization.differentiate              

              val movingGradientImage = movingImage.differentiate.get // TODO do proper error handling when image is not differentiable  
 
              // the first derivative (after applying the chain rule) at each point
              val parametricTransformGradient = (x: Point[D]) => {
                val domain = warpedImage.domain.intersection(dMetricDalpha.domain)
                if (domain.isDefinedAt(x)) 
                	Some(dTransformSpaceDAlpha(x).t * movingGradientImage(transformation(x)).toBreezeVector * dMetricDalpha(x))
                else None
              }
              
              val gradient = integrationStrategy.integrateVector(parametricTransformGradient, params.size)
             
              val dR = regularizer.takeDerivative(params)

              (value.toFloat, gradient + dR * configuration.regularizationWeight.toFloat)
            }
          }

          val optimizer = configuration.optimizer

          val optimalParameters = optimizer(configuration.initialParameters, costFunction)
          RegistrationResult(transformationSpace(optimalParameters), optimalParameters)

        }
    }

  def registration1D(configuration: RegistrationConfiguration[OneD])(
    fixedImage: ContinuousScalarImage1D,
    movingImage: ContinuousScalarImage1D): (BoxedDomain[OneD] => RegistrationResult[OneD]) =
    {
      registrationND(configuration)(fixedImage, movingImage)
    }

  def registration2D(configuration: RegistrationConfiguration[TwoD])(
    fixedImage: ContinuousScalarImage2D,
    movingImage: ContinuousScalarImage2D): (BoxedDomain[TwoD] => RegistrationResult[TwoD]) =
    {
      registrationND(configuration)(fixedImage, movingImage)
    }
  
  def registration3D(configuration: RegistrationConfiguration[ThreeD])(
    fixedImage: ContinuousScalarImage3D,
    movingImage: ContinuousScalarImage3D): (BoxedDomain[ThreeD] => RegistrationResult[ThreeD]) =
    {
      registrationND(configuration)(fixedImage, movingImage)
    }
}