/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalismo.registration

import TransformationSpace.ParameterVector
import breeze.linalg.{ DenseVector, convert }
import scalismo.geometry.{ _3D, Point, NDSpace, Dim }
import scalismo.image.{ DifferentiableScalarImage, ScalarImage }
import scalismo.numerics._
import scalismo.utils.Random

case class RegistrationConfiguration[D <: Dim: NDSpace, TS <: TransformationSpace[D] with DifferentiableTransforms[D]](
  optimizer: Optimizer,
  metric: ImageMetric[D],
  transformationSpace: TS,
  regularizer: Regularizer,
  regularizationWeight: Double)

object Registration {

  case class RegistrationState[D <: Dim, TS <: TransformationSpace[D] with DifferentiableTransforms[D]](registrationResult: TS#T, optimizerState: Optimizer#State)

  def iterations[D <: Dim: NDSpace, TS <: TransformationSpace[D] with DifferentiableTransforms[D]](config: RegistrationConfiguration[D, TS])(
    fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D],
    initialParameters: DenseVector[Double] = config.transformationSpace.identityTransformParameters): Iterator[RegistrationState[D, TS]] =
    {
      val regularizer = config.regularizer

      val transformationSpace = config.transformationSpace

      val costFunction = new CostFunction {
        def onlyValue(params: ParameterVector): Double = {
          val transformation = transformationSpace.transformForParameters(params)

          config.metric.value(movingImage, fixedImage, transformation) + config.regularizationWeight * regularizer(params)
        }
        def apply(params: ParameterVector): (Double, DenseVector[Double]) = {

          // create a new sampler, that simply caches the points and returns the same points in every call
          // this means, we are always using the same samples for computing the integral over the values
          // and the gradient
          val sampleStrategy = new SampleOnceSampler(config.metric.sampler)
          val integrationStrategy = Integrator[D](sampleStrategy)

          // compute the value of the cost function
          val transformation = Transformation.memoize(transformationSpace.transformForParameters(params), 100000)
          val errorVal = config.metric.value(movingImage, fixedImage, transformation)
          val value = errorVal + config.regularizationWeight * regularizer(params)

          // compute the derivative of the cost function
          val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)

          val metricDerivative = config.metric.takeDerivativeWRTToTransform(movingImage, fixedImage, transformation)
          // the first derivative (after applying the chain rule) at each point
          val parametricTransformGradient = (x: Point[D]) => metricDerivative(x).map {
            dM => convert(dTransformSpaceDAlpha(x).t, Float) * dM
          }
          val gradient = convert(integrationStrategy.integrateVector(parametricTransformGradient, params.size), Double)
          val dR = regularizer.takeDerivative(params)

          (value, gradient + dR * config.regularizationWeight)
        }
      }

      val optimizer = config.optimizer
      optimizer.iterations(initialParameters, costFunction).map {
        optimizerState =>
          val optParams = optimizerState.parameters
          val transformation = transformationSpace.transformForParameters(optParams)

          val regRes = transformation
          RegistrationState(regRes, optimizerState)
      }
    }

  def registration[D <: Dim: NDSpace, TS <: TransformationSpace[D] with DifferentiableTransforms[D]](configuration: RegistrationConfiguration[D, TS])(
    fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D]): TS#T = {
    val regStates = iterations(configuration)(fixedImage, movingImage)
    regStates.toSeq.last.registrationResult
  }

  // This class ensures that we are always getting the same points when we call sample.
  // This is important because we want the derivative to be evaluated at the same points as the
  // value of the metric, in our registration code.
  private case class SampleOnceSampler[D <: Dim](sampler: Sampler[D]) extends Sampler[D] {

    val numberOfPoints = sampler.numberOfPoints
    def volumeOfSampleRegion = sampler.volumeOfSampleRegion
    var lastSampledPoints: Option[IndexedSeq[(Point[D], Double)]] = None

    override def sample()(implicit rnd: Random): IndexedSeq[(Point[D], Double)] = {
      lastSampledPoints match {
        case Some(lastSampledPoints) => lastSampledPoints
        case None => {
          lastSampledPoints = Some(sampler.sample())
          lastSampledPoints.get
        }
      }
    }
  }

}