
package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.image.ContinuousScalarImage
import smptk.image.CoordVector
import smptk.image.DiscreteImageDomain

trait ImageMetric[CV[A] <: CoordVector[A], Repr] extends 
((ContinuousScalarImage[CV], ContinuousScalarImage[CV]) => (DiscreteImageDomain[CV] => Float)) {
  
  def takeDerivativeWRTToMovingImage(fixedImage : ContinuousScalarImage[CV], movingImage: ContinuousScalarImage[CV]) : ContinuousScalarImage[CV]  
}


object Metric {
}