
package smptk.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.numerics.Integration._
import smptk.image.ContinuousScalarImageLike
import smptk.image.ContinuousVectorImageLike
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.CostFunction
import smptk.image.CoordVectorLike

object Registration {

  case class RegistrationResult[Point <: CoordVectorLike](transform : Transformation[Point], parameters : ParameterVector) {}
  
  def registration[Point <: CoordVectorLike, Pixel](
    fixedImage: ContinuousScalarImageLike[Point],
    movingImage: ContinuousScalarImageLike[Point],
    transformationSpace: TransformationSpace[Point],
    metric: ImageMetric[Point],
    regularization: Regularizer,
    initialParameters: ParameterVector): RegistrationResult[Point] =
    {

      val costFunction = new CostFunction {

        def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

          val dTransformSpaceDAlpha = transformationSpace.takeDerivative(params)

          val warpedImage = new ContinuousScalarImageLike[Point] {
            def apply(pt: Point) = movingImage(transformationSpace(params)(pt))
            def domain = fixedImage.domain
            def takeDerivative(x: Point) = {
              val grad = movingImage.takeDerivative(transformationSpace(params)(x))
              dTransformSpaceDAlpha(x) * grad
            }
          }
          val value = metric(warpedImage, fixedImage) + regularization(params)
          val dMetricDalpha: ContinuousScalarImageLike[Point] = metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)
          val dMovingImageDAlpha: ContinuousVectorImageLike[Point] = warpedImage.differentiate

          val transformParameterGradientImage = new ContinuousVectorImageLike[Point] {
            val pixelDimensionality = params.size
            val domain = fixedImage.domain
            def apply(x: Point) =  warpedImage.takeDerivative(x) *  dMetricDalpha(x)
          }

          val gradient: DenseVector[Float] = integrate(transformParameterGradientImage)
          (value, gradient)
        }
      }

      val optimizer = GradientDescentOptimizer(100)
      
      val optimalParameters = optimizer(initialParameters, costFunction)
      RegistrationResult(transformationSpace(optimalParameters), optimalParameters)

    }
}