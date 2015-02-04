
package scalismo.registration

import scalismo.common.Domain
import scalismo.geometry._
import scalismo.image.{DifferentiableScalarImage, ScalarImage}
import scalismo.numerics.Integrator

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector


trait ImageMetric[D <: Dim] {
  def apply(img1: ScalarImage[D], img2: ScalarImage[D], transform: Transformation[D]): Double

  /**
   * Implmentations of this method should return the full derivations
   * i.e: (d/dMovingImage M(fixed,moving)(x)) * (d/dx(movingImage(transform(x))))
   */
  def takeDerivativeWRTToTransform(movingImage: DifferentiableScalarImage[D], fixedImage: ScalarImage[D], transform: Transformation[D]): Point[D] => Option[DenseVector[Float]]

}

//case class MeanSquaresMetricConfiguration extends MetricConfiguration 

abstract class MeanSquaresMetric[D <: Dim](val integrator: Integrator[D]) extends ImageMetric[D] {
  // val configuration : MetricConfiguration

  def apply(fixedImage: ScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
    val warpedImage = fixedImage.compose(transform)
    def square(img : ScalarImage[D]) = img :* img
    integrator.integrateScalar(square(warpedImage - movingImage)) / integrator.sampler.volumeOfSampleRegion
  }

  def takeDerivativeWRTToTransform(fixedImage: DifferentiableScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
    val movingGradientImage = fixedImage.differentiate
   val warpedImage = fixedImage.compose(transform)
    val dDMovingImage = (warpedImage - movingImage) * (2f / integrator.sampler.volumeOfSampleRegion)

 
    val fullMetricGradient = (x: Point[D]) => {
      val domain = Domain.intersection(warpedImage.domain,dDMovingImage.domain)
      if (domain.isDefinedAt(x))
        Some(movingGradientImage(transform(x)).toBreezeVector * dDMovingImage(x))
      else None
    }

    fullMetricGradient
  }
}

case class MeanSquaresMetric1D(override val integrator: Integrator[_1D]) extends MeanSquaresMetric[_1D](integrator)
case class MeanSquaresMetric2D(override val integrator: Integrator[_2D]) extends MeanSquaresMetric[_2D](integrator)
case class MeanSquaresMetric3D(override val integrator: Integrator[_3D]) extends MeanSquaresMetric[_3D](integrator)

object Metric {
}
