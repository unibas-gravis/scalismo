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
import scalismo.common.Domain
import scalismo.geometry.{ Point, NDSpace, Dim }
import scalismo.image.{ DifferentiableScalarImage, ScalarImage }
import scalismo.numerics.{ Integrator, Sampler }
import scalismo.registration.RegistrationMetric.ValueAndDerivative
import scalismo.utils.Random

/**
 * The mean squares image to image metric. The distance is computed by integrating over all the points
 * provided by the sampler.
 */
case class MeanSquaresMetric[D <: Dim: NDSpace](fixedImage: ScalarImage[D],
    movingImage: DifferentiableScalarImage[D],
    transformationSpace: TransformationSpace[D],
    sampler: Sampler[D])(implicit rng: Random) extends ImageMetric[D] {

  override val ndSpace = implicitly[NDSpace[D]]

  def value(parameters: DenseVector[Double]) = {
    val transform = transformationSpace.transformForParameters(parameters)
    computeValue(parameters, Integrator(sampler))
  }

  // compute the derivative of the cost function
  def derivative(parameters: DenseVector[Double]): DenseVector[Double] = {
    computeDerivative(parameters, Integrator(sampler))
  }

  override def valueAndDerivative(parameters: DenseVector[Double]): ValueAndDerivative = {

    // We create a new sampler, which always returns the same points. In this way we can make sure that the
    // same sample points are used for computing the value and the derivative
    val sampleOnceSampler = new Sampler[D] {
      override val numberOfPoints: Int = sampler.numberOfPoints
      private val samples = sampler.sample()

      override def sample()(implicit rand: Random): IndexedSeq[(Point[D], Double)] = samples
      override def volumeOfSampleRegion: Double = sampler.volumeOfSampleRegion
    }

    val integrator = Integrator(sampleOnceSampler)
    val value = computeValue(parameters, integrator)
    val derivative = computeDerivative(parameters, integrator)
    ValueAndDerivative(value, derivative)

  }

  private def computeValue(parameters: DenseVector[Double], integrator: Integrator[D]) = {

    val transform = transformationSpace.transformForParameters(parameters)

    val warpedImage = movingImage.compose(transform)
    def square(img: ScalarImage[D]) = img :* img
    integrator.integrateScalar(square(fixedImage - warpedImage)) / integrator.sampler.volumeOfSampleRegion
  }

  private def computeDerivative(parameters: DenseVector[Double],
    integrator: Integrator[D]): DenseVector[Double] = {

    val transform = transformationSpace.transformForParameters(parameters)

    val movingImageGradient = movingImage.differentiate
    val warpedImage = movingImage.compose(transform) // compute the derivative of the cost function
    val dDMovingImage = (fixedImage - warpedImage) * (-2.0 / sampler.volumeOfSampleRegion)

    val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(parameters)

    val fullMetricGradient = (x: Point[D]) => {
      val domain = Domain.intersection(fixedImage.domain, dDMovingImage.domain)
      if (domain.isDefinedAt(x))
        Some(dTransformSpaceDAlpha(x).t * (movingImageGradient(transform(x)) * dDMovingImage(x).toDouble).toBreezeVector)
      else None
    }

    integrator.integrateVector(fullMetricGradient, parameters.size)

  }

}
