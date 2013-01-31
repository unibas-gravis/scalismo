
package smptk.registration


import TransformationSpace.ParameterVector
import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.image.ContinuousScalarImageLike
import smptk.image.CoordVectorLike

trait ImageMetric[CoordVector[A] <: CoordVectorLike[A]] extends 
((ContinuousScalarImageLike[CoordVector], ContinuousScalarImageLike[CoordVector]) => Float) {
  
  def takeDerivativeWRTToMovingImage(fixedImage : ContinuousScalarImageLike[CoordVector], movingImage: ContinuousScalarImageLike[CoordVector]) : ContinuousScalarImageLike[CoordVector]  
}


object Metric {
}