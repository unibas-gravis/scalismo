
package org.statismo.stk.core.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import org.statismo.stk.core.image.ContinuousScalarImage
import org.statismo.stk.core.numerics.CostFunction
import org.statismo.stk.core.numerics.Optimizer
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.SampleOnceSampler
import org.statismo.stk.core.geometry._

case class RegistrationResult[D <: Dim](transform: Transformation[D], parameters: ParameterVector) {}

case class RegistrationConfiguration[D <: Dim : NDSpace](
                                                         optimizer: Optimizer,
                                                         integrator: Integrator[D],
                                                         metric: ImageMetric[D],
                                                         transformationSpace: TransformationSpace[D] with DifferentiableTransforms[D],
                                                         regularizer: Regularizer,
                                                         regularizationWeight: Double,
                                                         initialParametersOrNone: Option[DenseVector[Float]] = None) {
  def initialParameters = initialParametersOrNone.getOrElse(transformationSpace.identityTransformParameters)
}

object Registration {

  case class RegistrationState[D <: Dim](registrationResult: RegistrationResult[D], optimizerState: Optimizer#State)

  def iterations[D <: Dim : NDSpace](configuration: RegistrationConfiguration[D])(
    fixedImage: ContinuousScalarImage[D],
    movingImage: ContinuousScalarImage[D]): Iterator[RegistrationState[D]] = {
    val regularizer = configuration.regularizer

    val transformationSpace = configuration.transformationSpace

    val costFunction = new CostFunction {
      def onlyValue(params: ParameterVector): Double = {
        val transformation = transformationSpace.transformForParameters(params)

        configuration.metric(movingImage, fixedImage, transformation) + configuration.regularizationWeight * regularizer(params)
      }
      def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

        // create a new sampler, that simply caches the points and returns the same points in every call
        // this means, we are always using the same samples for computing the integral over the values
        // and the gradient
        val sampleStrategy = new SampleOnceSampler(configuration.integrator.sampler)
        val integrationStrategy = Integrator[D](IntegratorConfiguration(sampleStrategy))

        // compute the value of the cost function
        val transformation = transformationSpace.transformForParameters(params)
        val errorVal = configuration.metric(movingImage, fixedImage, transformation)
        val value = errorVal + configuration.regularizationWeight * regularizer(params)

        // compute the derivative of the cost function
        val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)

        val metricDerivative = configuration.metric.takeDerivativeWRTToTransform(movingImage, fixedImage, transformation)
        // the first derivative (after applying the chain rule) at each point
        val parametricTransformGradient = (x: Point[D]) => metricDerivative(x).map {
          dM => dTransformSpaceDAlpha(x).t * dM
        }
        val gradient = integrationStrategy.integrateVector(parametricTransformGradient, params.size)
        val dR = regularizer.takeDerivative(params)

        (value.toFloat, gradient + dR * configuration.regularizationWeight.toFloat)
      }
    }

    val optimizer = configuration.optimizer
    optimizer.iterations(configuration.initialParameters, costFunction).map {
      optimizerState =>
        val optParams = optimizerState.parameters
        val transformation = transformationSpace.transformForParameters(optParams)

        val regRes = RegistrationResult(transformation, optParams)
        RegistrationState(regRes, optimizerState)
    }
  }

  def registration[D <: Dim : NDSpace](configuration: RegistrationConfiguration[D])(
    fixedImage: ContinuousScalarImage[D],
    movingImage: ContinuousScalarImage[D]): RegistrationResult[D] = {
    val regStates = iterations(configuration)(fixedImage, movingImage)
    regStates.toSeq.last.registrationResult
  }
}