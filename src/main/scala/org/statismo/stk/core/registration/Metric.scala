
package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import smptk.geometry._
import smptk.image.ContinuousScalarImage
import smptk.image.DiscreteImageDomain
import smptk.numerics.Integrator
import smptk.common.BoxedDomain

trait MetricConfiguration 


trait ImageMetric[D <: Dim] {
  type Repr  = ContinuousScalarImage[D]
  def apply(img1: Repr, img2: Repr) :  (Integrator[D]) => Double

  def takeDerivativeWRTToMovingImage(fixedImage: Repr, movingImage: Repr): ContinuousScalarImage[D]
}

trait ImageMetric1D extends  ImageMetric[OneD] {
}

trait ImageMetric2D extends  ImageMetric[TwoD] {
}

trait ImageMetric3D extends  ImageMetric[ThreeD] {
}

//case class MeanSquaresMetricConfiguration extends MetricConfiguration 

trait MeanSquaresMetric[D <: Dim] extends ImageMetric[D] {
 // val configuration : MetricConfiguration
  type CImg = ContinuousScalarImage[D]

  def apply(img1: CImg,  img2: CImg) = {
    (integrator : Integrator[D]) => integrator.integrateScalar((img1 - img2).square)  
  }
  def takeDerivativeWRTToMovingImage(img1: CImg,  img2: CImg) = {
    (img1 - img2) * 2f
  }
  
}

case class MeanSquaresMetric1D() extends ImageMetric1D with MeanSquaresMetric[OneD] 
case class MeanSquaresMetric2D() extends ImageMetric2D with MeanSquaresMetric[TwoD]
case class MeanSquaresMetric3D() extends ImageMetric3D with MeanSquaresMetric[ThreeD]

object Metric {
}
