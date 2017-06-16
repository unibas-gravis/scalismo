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

case class RegistrationConfiguration[D <: Dim: NDSpace, TS <: TransformationSpace[D]](
  optimizer: Optimizer,
  metric: ImageMetric[D],
  transformationSpace: TS,
  regularizer: Regularizer,
  regularizationWeight: Double)

object Registration {

  case class RegistrationState[D <: Dim, TS <: TransformationSpace[D]](registrationResult: TS#T, optimizerState: Optimizer#State)

  def iterations[D <: Dim: NDSpace, TS <: TransformationSpace[D]](config: RegistrationConfiguration[D, TS])(
    fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D],
    initialParameters: DenseVector[Double] = config.transformationSpace.identityTransformParameters)(
      implicit rng: Random): Iterator[RegistrationState[D, TS]] =
    {
      val regularizer = config.regularizer

      val transformationSpace = config.transformationSpace

      val costFunction = new CostFunction {
        def onlyValue(params: ParameterVector): Double = {
          val transformation = transformationSpace.transformForParameters(params)

          config.metric.value(fixedImage, movingImage, transformation) + config.regularizationWeight * regularizer(params)
        }
        def apply(params: ParameterVector): (Double, DenseVector[Double]) = {

          // compute the value of the cost function
          val transformation = Transformation.memoize(transformationSpace.transformForParameters(params), 100000)
          val metricValueAndDerivative = config.metric.computeValueAndDerivative(fixedImage, movingImage, transformationSpace, params)
          val value = metricValueAndDerivative.value + config.regularizationWeight * regularizer(params)
          val dR = regularizer.takeDerivative(params)

          (value, metricValueAndDerivative.derivative + dR * config.regularizationWeight)
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

  def registration[D <: Dim: NDSpace, TS <: TransformationSpace[D]](configuration: RegistrationConfiguration[D, TS])(
    fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D])(
      implicit rng: Random): TS#T = {
    val regStates = iterations(configuration)(fixedImage, movingImage)
    regStates.toSeq.last.registrationResult
  }

}