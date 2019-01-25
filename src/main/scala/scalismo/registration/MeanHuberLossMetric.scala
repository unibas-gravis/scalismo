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

import scalismo.geometry.NDSpace
import scalismo.image.{ DifferentiableScalarImage, ScalarImage }
import scalismo.numerics._

/**
 * Image to image metric which applies the Huber Loss function to the pointwise pixel difference.
 * The parameter delta defines the threshold. The Huber loss increases quadratically for
 * values below this threshold and linearly for values above this threshold.
 * @see SumOfPointwiseLossMetric.
 *
 */
case class MeanHuberLossMetric[D: NDSpace](fixedImage: ScalarImage[D],
  movingImage: DifferentiableScalarImage[D],
  transformationSpace: TransformationSpace[D],
  sampler: Sampler[D],
  delta: Double = 1.345)
    extends MeanPointwiseLossMetric(fixedImage, movingImage, transformationSpace, sampler) {

  override protected def lossFunction(v: Float): Float = {
    if (v < delta)
      (v * v / 2f) / (1 + v * v)
    else
      (delta * (Math.abs(v) - delta / 2)).toFloat
  }

  override protected def lossFunctionDerivative(v: Float): Float = {
    if (v < delta) v else (delta * Math.signum(v)).toFloat
  }

}