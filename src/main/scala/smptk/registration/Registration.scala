
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
import smptk.image.DiscreteDomain
import smptk.image.DiscreteImageDomain
import smptk.image.ContinuousImageDomain
import smptk.image.ContinuousImageDomain

object Registration {

  case class RegistrationResult[CV[A] <: CoordVector[A]](transform: Transformation[CV], parameters: ParameterVector) {}

  def registration[CV[A] <: CoordVector[A], Repr, Pixel](
    fixedImage: ContinuousScalarImage[CV, Repr],
    movingImage: ContinuousScalarImage[CV, Repr],
    transformationSpace: TransformationSpace[CV],
    metric: ImageMetric[CV, Repr],
    regularization: Regularizer,
    initialParameters: ParameterVector): (DiscreteImageDomain[CV] => RegistrationResult[CV]) =
    {
      fixedImageRegion =>
        {
          val costFunction = new CostFunction {

            def apply(params: ParameterVector): (Float, DenseVector[Float]) = {
//
//              val dTransformSpaceDAlpha = transformationSpace.takeDerivative(params)
//
//              val warpedImage = new ContinuousScalarImage[CV, Repr] {
//                def f(pt: CV[Float]) = movingImage.f(transformationSpace(params)(pt))
//                def isDefinedAt(pt : CV[Float]) = fixedImage.isDefinedAt(pt) && movingImage.isDefinedAt(transformationSpace(params)(pt))
//                def df(x: CV[Float]) = {
//                  val grad = movingImage.df(transformationSpace(params)(x))
//                  dTransformSpaceDAlpha(x) * grad
//                }
//              }
//
//              val value = metric(warpedImage, fixedImage)(fixedImageRegion) + regularization(params)
//              val dMetricDalpha: ContinuousScalarImage[CV, Repr	] = metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)
//              val dMovingImageDAlpha: ContinuousVectorImage[CV] = warpedImage.differentiate
//
//              val transformParameterGradientImage = new ContinuousVectorImage[CV] {
//                val pixelDimensionality = params.size
//                def isDefinedAt(x : CV[Float]) = warpedImage.isDefinedAt(x)
//                def f(x: CV[Float]) = warpedImage.df(x) * dMetricDalpha(x)
//              }
//
//              val gradient: DenseVector[Float] = integrate(transformParameterGradientImage, fixedImageRegion)
              val value = 0
              val gradient = DenseVector.zeros[Float](fixedImageRegion.dimensionality)
              (value, gradient)
            }
          }

          val optimizer = GradientDescentOptimizer(100)

          val optimalParameters = optimizer(initialParameters, costFunction)
          RegistrationResult(transformationSpace(optimalParameters), optimalParameters)

        }
    }
}