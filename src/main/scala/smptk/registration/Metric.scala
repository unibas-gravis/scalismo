
package smptk.registration


import TransformationSpace.ParameterVector
import smptk.numerics.Integration._
import breeze.linalg.DenseVector
import smptk.image.ContinuousScalarImageLike
import smptk.image.{PointLike, VectorLike}

trait ImageMetric[Point <: PointLike, Vector <: VectorLike] extends 
((ContinuousScalarImageLike[Point, Vector], ContinuousScalarImageLike[Point, Vector]) => Float) {
  
  def takeDerivativeWRTToMovingImage(fixedImage : ContinuousScalarImageLike[Point, Vector], movingImage: ContinuousScalarImageLike[Point, Vector]) : ContinuousScalarImageLike[Point, Vector]  
}


object Metric {
}