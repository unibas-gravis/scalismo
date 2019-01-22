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
import breeze.linalg.{ DenseVector }
import scalismo.geometry.{ Dim }
import scalismo.numerics._

/**
 * Implementation of a gradient-based registration algorithm, whose cost function is defined by the sum
 * of a distance metric and a regularization term (weighted by a regularizationWeight).
 *
 * @param metric The distance metric used to compare the objects that should be registered.
 * @param regularizer The regularizer that is used
 * @param regularizationWeight A weight used to weight the influence of the regularizer (0 means the regularization term is not considered)
 * @param optimizer  The optimizer used to perform the minimization of the cost function
 */
case class Registration[D <: Dim](metric: RegistrationMetric[D],
    regularizer: Regularizer[D],
    regularizationWeight: Double,
    optimizer: Optimizer) {

  /**
   * Representation of the current state of the registration.
   * @param value The current value of the cost function
   * @param parameters The current parameters
   * @param optimizerState, more detailed information regarding the used optimizer.
   */
  case class RegistrationState(value: Double, parameters: DenseVector[Double], optimizerState: Optimizer#State)

  /**
   * Given a set of initial parameter, returns an iterator which can be used to drive the registration.
   */
  def iterator(initialParameters: DenseVector[Double]): Iterator[RegistrationState] =
    {

      val costFunction = new CostFunction {
        def onlyValue(params: ParameterVector): Double = {
          metric.value(params) + regularizationWeight * regularizer.value(params)
        }
        def apply(params: ParameterVector): (Double, DenseVector[Double]) = {

          // compute the value of the cost function
          val metricValueAndDerivative = metric.valueAndDerivative(params)
          val value = metricValueAndDerivative.value + regularizationWeight * regularizer.value(params)
          val dR = regularizer.takeDerivative(params)

          (value, metricValueAndDerivative.derivative + dR * regularizationWeight)
        }
      }

      optimizer.iterations(initialParameters, costFunction).map {
        optimizerState =>
          RegistrationState(optimizerState.value, optimizerState.parameters, optimizerState)
      }
    }

}