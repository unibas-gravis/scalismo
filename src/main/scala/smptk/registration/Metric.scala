
package smptk.registration


import TransformationSpace.ParameterVector
import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.image.ContinuousScalarImageLike
import smptk.image.CoordVectorLike

trait ImageMetric[Point <: CoordVectorLike] extends 
((ContinuousScalarImageLike[Point], ContinuousScalarImageLike[Point]) => Float) {
  
  def takeDerivativeWRTToMovingImage(fixedImage : ContinuousScalarImageLike[Point], movingImage: ContinuousScalarImageLike[Point]) : ContinuousScalarImageLike[Point]  
}


object Metric {
}