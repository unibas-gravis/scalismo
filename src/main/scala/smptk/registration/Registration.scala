
package smptk.registration

import scala.language.higherKinds
import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.numerics.Integration._
import smptk.image.ContinuousScalarImage
import smptk.image.ContinuousVectorImage
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.LBFGSOptimizer
import smptk.numerics.CostFunction
import smptk.image.CoordVector
import smptk.image.DiscreteDomain
import smptk.image.DiscreteImageDomain
import smptk.image.ContinuousImageDomain
import smptk.image.ContinuousImageDomain
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry._
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousScalarImage2D
import smptk.image.DiscreteImageDomain2D

object Registration {

  case class RegistrationResult[CV[A] <: CoordVector[A]](transform: Transformation[CV], parameters: ParameterVector) {}

  def registrationND[CV[A] <: CoordVector[A]](
    fixedImage: ContinuousScalarImage[CV],
    movingImage: ContinuousScalarImage[CV],
    transformationSpace: TransformationSpace[CV],
    metric: ImageMetric[CV],
    regWeight: Float,
    initialParameters: ParameterVector): (DiscreteImageDomain[CV] => RegistrationResult[CV]) =
    {
      fixedImageRegion =>
        {
          val regularizer = RKHSNormRegularizer

          val costFunction = new CostFunction {

            def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

              // compute the value of the cost function
              val transformation = transformationSpace(params)
              val warpedImage = movingImage.warp(transformation, fixedImage.isDefinedAt)
              val value = metric(warpedImage, fixedImage)(fixedImageRegion) + regWeight * regularizer(params)

              // compute the derivative of the cost function

              val dMetricDalpha = metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)

              val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)
              //TODO add reg val dRegularizationParam : DenseVector[Float] = regularization.differentiate              

              val parametricTransformGradientImage = new ContinuousVectorImage[CV] {
                val pixelDimensionality = params.size
                def isDefinedAt(x: CV[Float]) = warpedImage.isDefinedAt(x) && dMetricDalpha.isDefinedAt(x)
                def f(x: CV[Float]) = dTransformSpaceDAlpha(x).t * movingImage.df(transformation(x)) * dMetricDalpha(x)
              }

              val gradient: DenseVector[Float] = integrate(parametricTransformGradientImage, fixedImageRegion)
              val dR = regularizer.takeDerivative(params)

              (value, gradient + dR * regWeight)
            }
          }

          val optimizer = LBFGSOptimizer(100)
          //val optimizer = GradientDescentOptimizer(100)

          val optimalParameters = optimizer(initialParameters, costFunction)
          RegistrationResult(transformationSpace(optimalParameters), optimalParameters)

        }
    }

  def registration1D(
    fixedImage: ContinuousScalarImage1D,
    movingImage: ContinuousScalarImage1D,
    transformationSpace: TransformationSpace[CoordVector1D],
    metric: ImageMetric1D,
    regWeight: Float,
    initialParameters: ParameterVector): (DiscreteImageDomain1D => RegistrationResult[CoordVector1D]) =
    {
      registrationND(fixedImage, movingImage, transformationSpace, metric, regWeight, initialParameters)
    }
  
  def registration2D(
    fixedImage: ContinuousScalarImage2D,
    movingImage: ContinuousScalarImage2D,
    transformationSpace: TransformationSpace[CoordVector2D],
    metric: ImageMetric2D,
    regWeight: Float,
    initialParameters: ParameterVector): (DiscreteImageDomain2D => RegistrationResult[CoordVector2D]) =
    {
      registrationND(fixedImage, movingImage, transformationSpace, metric, regWeight, initialParameters)
    }
}