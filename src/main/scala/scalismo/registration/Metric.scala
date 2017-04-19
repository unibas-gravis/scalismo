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

import scalismo.common.Domain
import scalismo.geometry._
import scalismo.image.{ DifferentiableScalarImage, ScalarImage }
import scalismo.numerics.{ Sampler, Integrator }

import scala.language.higherKinds
import breeze.linalg.DenseVector

trait ImageMetric[D <: Dim] {

  /**
   * Computes the metric value M[img1 . transformation, img2] (where a . b denotes composition)
   */
  def value(img1: ScalarImage[D], img2: ScalarImage[D], transform: Transformation[D]): Double
  implicit def ndSpace: NDSpace[D]

  /**
   * Computes the derivative of the metric value M[img1 . transformation, img2], with respect to
   * the transform parameters, for the given transformation space and the transformation defined
   * at the current parameter values.
   */
  def takeDerivativeWRTToTransform(movingImage: DifferentiableScalarImage[D], fixedImage: ScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double]): DenseVector[Double]

}

case class MeanSquaresMetric[D <: Dim: NDSpace](sampler: Sampler[D]) extends ImageMetric[D] {

  private val integrator = Integrator(sampler)

  override val ndSpace = implicitly[NDSpace[D]]

  def value(fixedImage: ScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
    val warpedImage = fixedImage.compose(transform)
    def square(img: ScalarImage[D]) = img :* img

    integrator.integrateScalar(square(warpedImage - movingImage)) / integrator.sampler.volumeOfSampleRegion
  }
  // compute the derivative of the cost function
  def takeDerivativeWRTToTransform(fixedImage: DifferentiableScalarImage[D], movingImage: ScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double]): DenseVector[Double] = {

    val transform = transformationSpace.transformForParameters(params)

    val movingGradientImage = fixedImage.differentiate
    val warpedImage = fixedImage.compose(transform) // compute the derivative of the cost function
    val dDMovingImage = (warpedImage - movingImage) * (2f / sampler.volumeOfSampleRegion)

    val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)

    val fullMetricGradient = (x: Point[D]) => {
      val domain = Domain.intersection(warpedImage.domain, dDMovingImage.domain)
      if (domain.isDefinedAt(x))
        Some(dTransformSpaceDAlpha(x).t * (movingGradientImage(transform(x)) * dDMovingImage(x).toDouble).toBreezeVector)
      else None
    }

    integrator.integrateVector(fullMetricGradient, params.size)

  }
}
