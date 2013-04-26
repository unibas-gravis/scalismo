
package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import image._
import image.Geometry.{CoordVector1D,CoordVector2D, CoordVector3D}
import smptk.image.DiscreteImageDomain
import smptk.numerics.Integrator
import smptk.common.BoxedRegion

trait MetricConfiguration 


trait ImageMetric[CV[A] <: CoordVector[A]] {
  type Repr  = ContinuousScalarImage[CV]
  def apply(img1: Repr, img2: Repr) :  (Integrator[CV], BoxedRegion[CV]) => Double

  def takeDerivativeWRTToMovingImage(fixedImage: Repr, movingImage: Repr): ContinuousScalarImage[CV]
}

trait ImageMetric1D extends  ImageMetric[CoordVector1D] {
}

trait ImageMetric2D extends  ImageMetric[CoordVector2D] {
}

trait ImageMetric3D extends  ImageMetric[CoordVector3D] {
}

//case class MeanSquaresMetricConfiguration extends MetricConfiguration 

trait MeanSquaresMetric[CV[A] <: CoordVector[A]] extends ImageMetric[CV] {
 // val configuration : MetricConfiguration
  type CImg = ContinuousScalarImage[CV]

  def apply(img1: CImg,  img2: CImg) = {
    (integrator : Integrator[CV], region : BoxedRegion[CV]) => integrator.integrateScalar((img1 - img2).square, region)  
  }
  def takeDerivativeWRTToMovingImage(img1: CImg,  img2: CImg) = {
    (img1 - img2) * 2f
  }
  
}

case class MeanSquaresMetric1D() extends ImageMetric1D with MeanSquaresMetric[CoordVector1D] 
case class MeanSquaresMetric2D() extends ImageMetric2D with MeanSquaresMetric[CoordVector2D]
case class MeanSquaresMetric3D() extends ImageMetric3D with MeanSquaresMetric[CoordVector3D]

object Metric {
}
