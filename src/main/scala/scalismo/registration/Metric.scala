
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
