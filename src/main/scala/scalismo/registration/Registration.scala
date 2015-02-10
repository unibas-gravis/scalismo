
package scalismo.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import scalismo.geometry.{Point, NDSpace, Dim}
import scalismo.image.{DifferentiableScalarImage, ScalarImage}
import scalismo.numerics._

case class RegistrationResult[D <: Dim](transform: Transformation[D], parameters: ParameterVector) {}

case class RegistrationConfiguration[D <: Dim : NDSpace](
                                                         optimizer: Optimizer,
                                                         metric: ImageMetric[D],
                                                         transformationSpace: TransformationSpace[D] with DifferentiableTransforms[D],
                                                         regularizer: Regularizer,
                                                         regularizationWeight: Double)


object Registration {

  case class RegistrationState[D <: Dim](registrationResult: RegistrationResult[D], optimizerState: Optimizer#State)

  def iterations[D <: Dim : NDSpace](config: RegistrationConfiguration[D])(
    fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D],
    initialParameters: DenseVector[Float] = config.transformationSpace.identityTransformParameters): Iterator[RegistrationState[D]] =
  {
    val regularizer = config.regularizer

    val transformationSpace = config.transformationSpace

    val costFunction = new CostFunction {
      def onlyValue(params: ParameterVector): Double = {
        val transformation = transformationSpace.transformForParameters(params)

        config.metric.value(movingImage, fixedImage, transformation) + config.regularizationWeight * regularizer(params)
      }
      def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

        // create a new sampler, that simply caches the points and returns the same points in every call
        // this means, we are always using the same samples for computing the integral over the values
        // and the gradient
        val sampleStrategy = new SampleOnceSampler(config.metric.sampler)
        val integrationStrategy = Integrator[D](sampleStrategy)

        // compute the value of the cost function
        val transformation = transformationSpace.transformForParameters(params)
        val errorVal = config.metric.value(movingImage, fixedImage, transformation)
        val value = errorVal + config.regularizationWeight * regularizer(params)

        // compute the derivative of the cost function
        val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)

        val metricDerivative = config.metric.takeDerivativeWRTToTransform(movingImage, fixedImage, transformation)
        // the first derivative (after applying the chain rule) at each point
        val parametricTransformGradient = (x: Point[D]) => metricDerivative(x).map {
          dM => dTransformSpaceDAlpha(x).t * dM
        }
        val gradient = integrationStrategy.integrateVector(parametricTransformGradient, params.size)
        val dR = regularizer.takeDerivative(params)

        (value.toFloat, gradient + dR * config.regularizationWeight.toFloat)
      }
    }

    val optimizer = config.optimizer
    optimizer.iterations(initialParameters, costFunction).map {
      optimizerState =>
        val optParams = optimizerState.parameters
        val transformation = transformationSpace.transformForParameters(optParams)

        val regRes = RegistrationResult(transformation, optParams)
        RegistrationState(regRes, optimizerState)
    }
  }

  def registration[D <: Dim : NDSpace](configuration: RegistrationConfiguration[D])(
    fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D]): RegistrationResult[D] = {
    val regStates = iterations(configuration)(fixedImage, movingImage)
    regStates.toSeq.last.registrationResult
  }

  // This class ensures that we are always getting the same points when we call sample.
  // This is important because we want the derivative to be evaluated at the same points as the
  // value of the metric, in our registration code.
  private case class SampleOnceSampler[D <: Dim](sampler: Sampler[D]) extends Sampler[D] {

    val numberOfPoints = sampler.numberOfPoints
    def volumeOfSampleRegion = sampler.volumeOfSampleRegion
    override lazy val sample: IndexedSeq[(Point[D], Double)] = sampler.sample
  }


}