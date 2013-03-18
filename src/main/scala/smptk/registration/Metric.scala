
package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import numerics.Integration
import breeze.linalg.DenseVector
import image._
import image.Geometry.{CoordVector1D,CoordVector2D, CoordVector3D}
import smptk.image.DiscreteImageDomain

trait ImageMetric[CV[A] <: CoordVector[A]] {
  type Repr  = ContinuousScalarImage[CV]

  def apply(img1: Repr, img2: Repr): (DiscreteImageDomain[CV] => Double)

  def takeDerivativeWRTToMovingImage(fixedImage: Repr, movingImage: Repr): ContinuousScalarImage[CV]
}

trait ImageMetric1D extends  ImageMetric[CoordVector1D] {
}

trait ImageMetric2D extends  ImageMetric[CoordVector2D] {
}

trait ImageMetric3D extends  ImageMetric[CoordVector3D] {
}


trait MeanSquaresMetric[CV[A] <: CoordVector[A]] extends ImageMetric[CV] {
  type CImg = ContinuousScalarImage[CV]
  def apply(img1: CImg,  img2: CImg) = {
    (region : DiscreteImageDomain[CV]) => Integration.integrate((img1 - img2).square, region)
  }
  def takeDerivativeWRTToMovingImage(img1: CImg,  img2: CImg) = {
    (img1 - img2) * 2f
  }
}

object MeanSquaresMetric1D extends ImageMetric1D with MeanSquaresMetric[CoordVector1D]
object MeanSquaresMetric2D extends ImageMetric2D with MeanSquaresMetric[CoordVector2D]
object MeanSquaresMetric3D extends ImageMetric3D with MeanSquaresMetric[CoordVector3D]

object Metric {
}