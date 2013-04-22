
package smptk
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import image._
import image.Geometry.{CoordVector1D,CoordVector2D, CoordVector3D}
import smptk.image.DiscreteImageDomain
import numerics.UniformIntegrator
import numerics.UniformIntegratorConfiguration

trait MetricConfiguration 


case class StochasticMetricConfiguration(val numPoints : Int) extends MetricConfiguration 

trait ImageMetric[CV[A] <: CoordVector[A]] {
  type Repr  = ContinuousScalarImage[CV]

  def apply(img1: Repr, img2: Repr): (DiscreteImageDomain[CV] => (Double, IndexedSeq[CV[Double]]))

  def takeDerivativeWRTToMovingImage(fixedImage: Repr, movingImage: Repr): ContinuousScalarImage[CV]
}

trait ImageMetric1D extends  ImageMetric[CoordVector1D] {
}

trait ImageMetric2D extends  ImageMetric[CoordVector2D] {
}

trait ImageMetric3D extends  ImageMetric[CoordVector3D] {
}


case class MeanSquaresMetricConfiguration extends MetricConfiguration 

trait MeanSquaresMetric[CV[A] <: CoordVector[A]] extends ImageMetric[CV] {
  val configuration : MetricConfiguration
  
  type CImg = ContinuousScalarImage[CV]
  val integrator = UniformIntegrator[CV]( UniformIntegratorConfiguration(2000) )
  def apply(img1: CImg,  img2: CImg) = {
    // TODO : incoherence here :   the returned points are not the ones on which the integration has been computed !
    (region : DiscreteImageDomain[CV]) => ( integrator.integrateScalar((img1 - img2).square, region), region.points)  
  }
  def takeDerivativeWRTToMovingImage(img1: CImg,  img2: CImg) = {
    (img1 - img2) * 2f
  }
}


trait StochasticSquaresMetric[CV[A] <: CoordVector[A]] extends ImageMetric[CV]{  
  val configuration : StochasticMetricConfiguration
  
  type CImg = ContinuousScalarImage[CV]
   def apply(img1: CImg,  img2: CImg) = {
    (region : DiscreteImageDomain[CV]) => {
    
      val sampledPoints = region.uniformDistributionRandomSamples(configuration.numPoints)
      val squaredDiffImage = (img1-img2).square
      val v = sampledPoints.map(p=> squaredDiffImage.liftPixelValue(p).getOrElse(0.)).sum

      (v, sampledPoints)
    }
  }
  def takeDerivativeWRTToMovingImage(img1: CImg,  img2: CImg) = {
    (img1 - img2) * 2f
  }
}

case class StochasticSquaresMetric2D(configuration : StochasticMetricConfiguration)  extends ImageMetric2D with StochasticSquaresMetric[CoordVector2D]


case class MeanSquaresMetric1D(configuration : MetricConfiguration) extends ImageMetric1D with MeanSquaresMetric[CoordVector1D]
case class MeanSquaresMetric2D(configuration : MetricConfiguration) extends ImageMetric2D with MeanSquaresMetric[CoordVector2D]
case class MeanSquaresMetric3D(configuration : MetricConfiguration) extends ImageMetric3D with MeanSquaresMetric[CoordVector3D]

object Metric {
}
