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

import breeze.linalg.DenseVector
import scalismo.common.{DifferentiableField, Field, Scalar}
import scalismo.geometry._
import scalismo.registration.RegistrationMetric.ValueAndDerivative
import scalismo.transformations.TransformationSpace

/**
 * The basic interface for defining a metric for the scalismo registration framework.
 * It is independent of the object representation that is used. The main assumption is
 * that the objects are subject to a parametric transformations, and we can obtain for each
 * fixed value of the parameters compute the value of the metric and a derivative.
 *
 */
trait RegistrationMetric[D] {

  def transformationSpace: TransformationSpace[D]
  implicit def ndSpace: NDSpace[D]

  /**
   * Computes the metric value for the given parameter vector
   */
  def value(parameters: DenseVector[Double]): Double

  /**
   * Computes the derivative of the metric at the point given by the parameters.
   */
  def derivative(parameters: DenseVector[Double]): DenseVector[Double]

  /**
   * Computes value and derivative in one go. It should be the same as calling value and takeDerivative
   * separately, but allows for more efficient implementations.
   *
   */
  def valueAndDerivative(parameters: DenseVector[Double]): ValueAndDerivative
}

object RegistrationMetric {

  case class ValueAndDerivative(value: Double, derivative: DenseVector[Double])
}

/**
 * A registration metric for image to image registration.
 */
trait ImageMetric[D, A] extends RegistrationMetric[D] {

  def scalar: Scalar[A]

  def fixedImage: Field[D, A]

  def movingImage: DifferentiableField[D, A]

}
