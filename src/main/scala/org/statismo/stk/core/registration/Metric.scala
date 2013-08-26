
package org.statismo.stk.core
package registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.image.ContinuousScalarImage
import org.statismo.stk.core.image.DiscreteImageDomain
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.common.BoxedDomain

trait MetricConfiguration 


trait ImageMetric[D <: Dim] {
  type Repr  = ContinuousScalarImage[D]
  def apply(img1: Repr, img2: Repr) : Double

  def takeDerivativeWRTToMovingImage(fixedImage: Repr, movingImage: Repr): ContinuousScalarImage[D]
}



//case class MeanSquaresMetricConfiguration extends MetricConfiguration 

abstract class MeanSquaresMetric[D <: Dim](val integrator : Integrator[D]) extends ImageMetric[D] {
 // val configuration : MetricConfiguration
  type CImg = ContinuousScalarImage[D]
  
  def apply(img1: CImg,  img2: CImg) = {
   integrator.integrateScalar((img1 - img2).square) / integrator.sampler.volumeOfSampleRegion
  }
  def takeDerivativeWRTToMovingImage(img1: CImg,  img2: CImg) = {
    (img1 - img2) * (2f  / integrator.sampler.volumeOfSampleRegion)
  }
  
}

case class MeanSquaresMetric1D(override val integrator: Integrator[OneD]) extends  MeanSquaresMetric[OneD](integrator)
case class MeanSquaresMetric2D(override val integrator: Integrator[TwoD]) extends MeanSquaresMetric[TwoD](integrator)
case class MeanSquaresMetric3D(override val integrator: Integrator[ThreeD]) extends MeanSquaresMetric[ThreeD](integrator)

object Metric {
}
