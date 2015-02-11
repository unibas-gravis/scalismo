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
import scalismo.image.{DifferentiableScalarImage, ScalarImage}
import scalismo.numerics.{Sampler, Integrator}

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector


trait ImageMetric[D <: Dim] {
  def value(img1: ScalarImage[D], img2: ScalarImage[D], transform: Transformation[D]): Double
  implicit def ndSpace : NDSpace[D]

  /**
   * Implmentations of this method should return the full derivations
   * i.e: (d/dMovingImage M(fixed,moving)(x)) * (d/dx(movingImage(transform(x))))
   */
  def takeDerivativeWRTToTransform(movingImage: DifferentiableScalarImage[D], fixedImage: ScalarImage[D], transform: Transformation[D]): Point[D] => Option[DenseVector[Float]]
  def sampler : Sampler[D]
  val integrator = Integrator(sampler)
}

//case class MeanSquaresMetricConfiguration extends MetricConfiguration 

case class MeanSquaresMetric[D <: Dim : NDSpace](val sampler: Sampler[D]) extends ImageMetric[D] {
  // val configuration : MetricConfiguration

  override val ndSpace = implicitly[NDSpace[D]]

  def value(fixedImage: ScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
    val warpedImage = fixedImage.compose(transform)
    def square(img : ScalarImage[D]) = img :* img

    integrator.integrateScalar(square(warpedImage - movingImage)) / integrator.sampler.volumeOfSampleRegion
  }

  def takeDerivativeWRTToTransform(fixedImage: DifferentiableScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
    val movingGradientImage = fixedImage.differentiate
   val warpedImage = fixedImage.compose(transform)
    val dDMovingImage = (warpedImage - movingImage) * (2f / sampler.volumeOfSampleRegion)

 
    val fullMetricGradient = (x: Point[D]) => {
      val domain = Domain.intersection(warpedImage.domain,dDMovingImage.domain)
      if (domain.isDefinedAt(x))
        Some(movingGradientImage(transform(x)).toBreezeVector * dDMovingImage(x))
      else None
    }

    fullMetricGradient
  }
}
