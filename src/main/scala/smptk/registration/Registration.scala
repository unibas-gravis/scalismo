
package smptk.registration

import scala.language.higherKinds

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

  case class RegistrationResult[CoordVector[A] <: CoordVectorLike[A]](transform : Transformation[CoordVector], parameters : ParameterVector) {}
  
  def registration[CoordVector[A] <: CoordVectorLike[A], Pixel](
    fixedImage: ContinuousScalarImageLike[CoordVector],
    movingImage: ContinuousScalarImageLike[CoordVector],
    transformationSpace: TransformationSpace[CoordVector],
    metric: ImageMetric[CoordVector],
    regularization: Regularizer,
    initialParameters: ParameterVector): RegistrationResult[CoordVector] =
    {

      val costFunction = new CostFunction {

        def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

          val dTransformSpaceDAlpha = transformationSpace.takeDerivative(params)

          val warpedImage = new ContinuousScalarImageLike[CoordVector] {
            def apply(pt: CoordVector[Float]) = movingImage(transformationSpace(params)(pt))
            def domain = fixedImage.domain
            def takeDerivative(x: CoordVector[Float]) = {
              val grad = movingImage.takeDerivative(transformationSpace(params)(x))
              dTransformSpaceDAlpha(x) * grad
            }
          }
          val value = metric(warpedImage, fixedImage) + regularization(params)
          val dMetricDalpha: ContinuousScalarImageLike[CoordVector] = metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)
          val dMovingImageDAlpha: ContinuousVectorImageLike[CoordVector] = warpedImage.differentiate

          val transformParameterGradientImage = new ContinuousVectorImageLike[CoordVector] {
            val pixelDimensionality = params.size
            val domain = fixedImage.domain
            def apply(x: CoordVector[Float]) =  warpedImage.takeDerivative(x) *  dMetricDalpha(x)
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