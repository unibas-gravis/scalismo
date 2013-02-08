
package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.image.ContinuousScalarImage
import smptk.image.CoordVector
import smptk.image.DiscreteImageDomain

trait ImageMetric[CV[A] <: CoordVector[A], Repr] extends 
((ContinuousScalarImage[CV, Repr], ContinuousScalarImage[CV, Repr]) => (DiscreteImageDomain[CV] => Float)) {
  
  def takeDerivativeWRTToMovingImage(fixedImage : ContinuousScalarImage[CV, Repr], movingImage: ContinuousScalarImage[CV, Repr]) : ContinuousScalarImage[CV, Repr]  
}


object Metric {
}