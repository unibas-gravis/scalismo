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
import scalismo.numerics._

/**
 * Image to image metric which applies the Huber Loss function to the pointwise pixel difference.
 * The parameter delta defines the threshold. The Huber loss increases quadratically for
 * values below this threshold and linearly for values above this threshold.
 * @see SumOfPointwiseLossMetric.
 *
 */
case class MeanHuberLossMetric[D: NDSpace, A: Scalar](
  fixedImage: ScalarImage[D, A],
  movingImage: DifferentiableScalarImage[D, A],
  transformationSpace: TransformationSpace[D],
  sampler: Sampler[D],
  delta: Double = 1.345)
  extends MeanPointwiseLossMetric(fixedImage, movingImage, transformationSpace, sampler) {

  val scalar = Scalar[A]
  override protected def lossFunction(v: A): Double = {
    val vAsDouble = scalar.toDouble(v)
    if (vAsDouble < delta)
      (vAsDouble * vAsDouble / 2f)
    else
      (delta * (Math.abs(vAsDouble) - delta / 2)).toFloat
  }

  override protected def lossFunctionDerivative(v: A): Double = {
    val vAsDouble = scalar.toDouble(v)
    if (vAsDouble < delta) vAsDouble else (delta * Math.signum(vAsDouble)).toFloat
  }

}