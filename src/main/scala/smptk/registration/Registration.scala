
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
import smptk.image.ContinuousScalarImage1D
import smptk.image.Geometry._
import smptk.image.DiscreteImageDomain1D

object Registration {

  case class RegistrationResult[CV[A] <: CoordVector[A]](transform: Transformation[CV], parameters: ParameterVector) {}

  def registration1D(
    fixedImage: ContinuousScalarImage1D,
    movingImage: ContinuousScalarImage1D,
    transformationSpace: TransformationSpace[CoordVector1D],
    metric: ImageMetric1D,
    regularization: Regularizer,
    initialParameters: ParameterVector): (DiscreteImageDomain1D => RegistrationResult[CoordVector1D]) =
    {
      fixedImageRegion =>
        {
          val costFunction = new CostFunction {

            def apply(params: ParameterVector): (Float, DenseVector[Float]) = {

              // compute the value of the cost function
              val transformation = transformationSpace(params)
              val warpedImage   = movingImage.warp(transformation, fixedImage.isDefinedAt)
              val value = metric(warpedImage, fixedImage)(fixedImageRegion) + regularization(params)

              // compute the derivative of the cost function
              
              val dMetricDalpha = metric.takeDerivativeWRTToMovingImage(warpedImage, fixedImage)

              val dTransformSpaceDAlpha = transformationSpace.takeDerivativeWRTParameters(params)
              //TODO add reg val dRegularizationParam : DenseVector[Float] = regularization.differentiate              

              val parametricTransformGradientImage = new ContinuousVectorImage[CoordVector1D] {
                val pixelDimensionality = params.size
                def isDefinedAt(x : Point1D) = warpedImage.isDefinedAt(x) && dMetricDalpha.isDefinedAt(x) 
                def f(x: Point1D) =  dTransformSpaceDAlpha(x) * movingImage.df(transformation(x))* dMetricDalpha(x)  
              }
              

              val gradient: DenseVector[Float] = integrate(parametricTransformGradientImage, fixedImageRegion)
              
              val dMovingImageDAlpha = warpedImage.differentiate



              (value, gradient)
            }
          }

          val optimizer = GradientDescentOptimizer(100)

          val optimalParameters = optimizer(initialParameters, costFunction)
          RegistrationResult(transformationSpace(optimalParameters), optimalParameters)

        }
    }
}