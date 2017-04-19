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
import scalismo.registration.ImageMetric.ValueAndDerivative
import scalismo.utils.Random

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
  def takeDerivativeWRTToTransform(im1: ScalarImage[D], img2: DifferentiableScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double]): DenseVector[Double]

  /**
   * Computes value and derivative in one go.
   */
  def computeValueAndDerivative(img1: ScalarImage[D], img2: DifferentiableScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double]): ValueAndDerivative
}

object ImageMetric {
  case class ValueAndDerivative(value: Double, derivative: DenseVector[Double])
}

case class MeanSquaresMetric[D <: Dim: NDSpace](sampler: Sampler[D]) extends ImageMetric[D] {

  override val ndSpace = implicitly[NDSpace[D]]

  def value(fixedImage: ScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
    computeValue(fixedImage, movingImage, transform, Integrator(sampler))
  }

  // compute the derivative of the cost function
  def takeDerivativeWRTToTransform(fixedImage: ScalarImage[D], movingImage: DifferentiableScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double]): DenseVector[Double] = {
    computeDerivative(fixedImage, movingImage, transformationSpace, params, Integrator(sampler))
  }

  override def computeValueAndDerivative(fixedImage: ScalarImage[D], movingImage: DifferentiableScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double]): ValueAndDerivative = {

    // We create a new sampler, which always returns the same points. In this way we can make sure that the
    // same sample points are used for computing the value and the derivative
    val sampleOnceSampler = new Sampler[D] {
      override val numberOfPoints: Int = sampler.numberOfPoints
      private val samples = sampler.sample()

      override def sample()(implicit rand: Random): IndexedSeq[(Point[D], Double)] = samples
      override def volumeOfSampleRegion: Double = sampler.volumeOfSampleRegion
    }

    val integrator = Integrator(sampleOnceSampler)
    val value = computeValue(fixedImage, movingImage, transformationSpace.transformForParameters(params), integrator)
    val derivative = computeDerivative(fixedImage, movingImage, transformationSpace, params, integrator)
    ValueAndDerivative(value, derivative)

  }

  private def computeValue(fixedImage: ScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D], integrator: Integrator[D]) = {
    val warpedImage = movingImage.compose(transform)
    def square(img: ScalarImage[D]) = img :* img

    integrator.integrateScalar(square(fixedImage - warpedImage)) / integrator.sampler.volumeOfSampleRegion
  }

  private def computeDerivative(fixedImage: ScalarImage[D], movingImage: DifferentiableScalarImage[D],
    transformationSpace: TransformationSpace[D], params: DenseVector[Double],
    integrator: Integrator[D]): DenseVector[Double] = {

    val transform = transformationSpace.transformForParameters(params)

    val movingImageGradient = movingImage.differentiate
    val warpedImage = movingImage.compose(transform) // compute the derivative of the cost function
    val dDMovingImage = (fixedImage - warpedImage) * (-2.0 / sampler.volumeOfSampleRegion)

    val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)

    val fullMetricGradient = (x: Point[D]) => {
      val domain = Domain.intersection(fixedImage.domain, dDMovingImage.domain)
      if (domain.isDefinedAt(x))
        Some(dTransformSpaceDAlpha(x).t * (movingImageGradient(transform(x)) * dDMovingImage(x).toDouble).toBreezeVector)
      else None
    }

    integrator.integrateVector(fullMetricGradient, params.size)

  }

}
