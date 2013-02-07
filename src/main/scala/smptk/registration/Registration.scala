
package smptk.registration

import scala.language.higherKinds

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.numerics.Integration._
import smptk.image.ContinuousScalarImage
import smptk.image.ContinuousVectorImage
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.CostFunction
import smptk.image.CoordVector

object Registration {

  case class RegistrationResult[CV[A] <: CoordVector[A]](transform : Transformation[CV], parameters : ParameterVector) {}
  
  def registration[CV[A] <: CoordVector[A], Pixel](
    fixedImage: ContinuousScalarImage[CV],
    movingImage: ContinuousScalarImage[CV],
    transformationSpace: TransformationSpace[CV],
    metric: ImageMetric[CV],
    regularization: Regularizer,
    initialParameters: ParameterVector): RegistrationResult[CV] =
    {

      val costFunction = new CostFunction {

        def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

          val dTransformSpaceDAlpha = transformationSpace.takeDerivative(params)

          val warpedImage = new ContinuousScalarImage[CV] {
            def f(pt: CV[Float]) = movingImage.f(transformationSpace(params)(pt))
            def domain = fixedImage.domain
            def df(x: CV[Float]) = {
              val grad = movingImage.df(transformationSpace(params)(x))
              dTransformSpaceDAlpha(x) * grad
            }
          }
          

          val value = metric(warpedImage, fixedImage) + regularization(params)
          val dMetricDalpha: ContinuousScalarImage[CV] = metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)
          val dMovingImageDAlpha: ContinuousVectorImage[CV] = warpedImage.differentiate

          val transformParameterGradientImage = new ContinuousVectorImage[CV] {
        val pixelDimensionality = params.size
            val domain = fixedImage.domain
            def f(x: CV[Float]) =  warpedImage.df(x) *  dMetricDalpha(x)
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