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

import scalismo.common.Scalar
import scalismo.geometry.NDSpace
import scalismo.image.{ DifferentiableScalarImage, ScalarImage }
import scalismo.numerics.Sampler

/**
 * The mean squares image to image metric.
 * It is implemented as the squared loss function in terms of the pointwise pixel differences.
 */
case class MeanSquaresMetric[D: NDSpace, A: Scalar](
  fixedImage: ScalarImage[D, A],
  movingImage: DifferentiableScalarImage[D, A],
  transformationSpace: TransformationSpace[D],
  sampler: Sampler[D])
  extends MeanPointwiseLossMetric[D, A](fixedImage, movingImage, transformationSpace, sampler) {

  private val scalar = Scalar[A]

  override val ndSpace = implicitly[NDSpace[D]]

  override protected def lossFunction(v: A): Double = {
    val value = scalar.toDouble(v)
    value * value;
  }

  override protected def lossFunctionDerivative(v: A): Double = {
    2.0 * scalar.toDouble(v)
  }

}
