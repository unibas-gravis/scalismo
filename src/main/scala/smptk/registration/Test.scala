package smptk.registration

import scala.collection.immutable.{ Vector => SVector }
import TransformationSpace.ParameterVector
import Registration.RegistrationResult
import breeze.plot._
import breeze.linalg._
import smptk.image.ContinuousScalarImage1D
import smptk.image.ContinuousDomain
import smptk.image.Geometry._
import smptk.image.ContinuousImageDomain1D
import smptk.image.ContinuousScalarImage
import smptk.image.DiscreteScalarImage1D
import smptk.image.DiscreteImageDomain1D
import smptk.image.Image._
import smptk.image.Interpolation._
import smptk.image.ContinuousImageDomain
import smptk.numerics.Integration
import smptk.image.ContinuousScalarImage1D

// object Test {

//  // a concrete example of a registration
//  def doRegistration(): RegistrationResult[CoordVector1D] = {
//
//    val oneDTransformSpace = TranslationSpace1D()
//    val range = (-15 until 15).toIndexedSeq
//
//    val discreteFixedImage = DiscreteScalarImage1D(DiscreteImageDomain1D(-15f, 1f, 30), range.map(Math.pow(_, 2)).toArray.toIndexedSeq)
//    val fixedImage = interpolate(3)(discreteFixedImage)
//
//    val discreteMovingImage = DiscreteScalarImage1D(DiscreteImageDomain1D(-15f, 1f, 30), range.map(1.4 * Math.pow(_, 2)).toArray.toIndexedSeq)
//    val movingImage = interpolate(3)(discreteMovingImage)
//
//    val metric = new ImageMetric[CoordVector1D, ContinuousScalarImage1D] {
//      def apply(img1: ContinuousScalarImage1D,
//        img2: ContinuousScalarImage1D) = { 
//        (region : DiscreteImageDomain1D) => {
//        val squaredImg = (img1 - img2) :* (img1 - img2)
//        Integration.integrate(squaredImg, region)
//        }
//      }
//      
//      def takeDerivativeWRTToMovingImage(img1: ContinuousScalarImage1D,
//        img2: ContinuousScalarImage1D) = {
//        (img1 - img2) * 2f
//      }
//    }
//
//    val initialParameters = DenseVector(1f)
//    val regularizer = new Regularizer {
//      def apply(p: ParameterVector) = 0f
//    }
//
//    val regResult = Registration.registration(fixedImage, movingImage, oneDTransformSpace, metric, regularizer, initialParameters)(discreteFixedImage.domain)
//
//    val f = Figure()
//    val p = f.subplot(0)
//
//    val warpedImage = new ContinuousScalarImage[CoordVector1D, ContinuousScalarImage1D] {
//      def f(pt: Point1D) = movingImage.f(regResult.transform(pt))
//      def domain = new ContinuousImageDomain[CoordVector1D] {
//        val isInside = (pt: CoordVector1D[Float]) => fixedImage.isDefinedAt(pt) && movingImage.isDefinedAt(regResult.transform(pt))
//      }
//      def df(x: CoordVector1D[Float]) = {
//        val grad = movingImage.df(regResult.transform(x))
//        grad
//      }
//    }
//
//    val warpedImgValueReplaced = warpedImage.liftPixelValue
//    
//    val x = discreteFixedImage.domain.points.map(_.toFloat)
//    p += plot(x, x.map(f => fixedImage(f)))
//    p += plot(x, x.map(f => warpedImgValueReplaced(f).getOrElse(0f)), '+')
//
//    // 
//    //    p += plot(x, x.map(f => fixedImage(f)))
//    //    val warpedimg = movingImage compose regResult.transform
//    //    p += plot(x, x.map(f => warpedimg(f)), '+')
//
//    //    val alpha : DenseVector[Float]= linspace(-1, 1).map(_.toFloat)
//    //    def warpedImage(alpha : DenseVector[Float]) =[Space] { 
//    //      def apply(x : Point1D) = movingImage(oneDTransformSpace(alpha)(x))
//    //      def takeDerivative(x : Space#Point) = DenseVector(0)
//    //      def domain = fixedImage.domain
//    //    }
//    //    def metricValue(alpha : Float) = metric(fixedImage, warpedImage(DenseVector(alpha))) 
//    //    p += plot(alpha,  alpha.map(metricValue))
//    //    
//    //    val img = new ContinuousVectorImage[Space] {
//    //      val pixelDimensionality = 2
//    //      val domain = Simple1DDomain(-1, DenseVector(2f))
//    //      def apply(x: Space#Point) = DenseVector(x * x, x* x)
//    //      def takeDerivative(x: Space#Point) = DenseVector(0)
//    //    }
//
//    regResult
//  }

//  def main(args: Array[String]) {
//    val regResult = doRegistration()
//    println("optimal parameters: " + regResult.parameters.toString)

//  }

//}