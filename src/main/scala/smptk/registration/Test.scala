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
import smptk.io.ImageIO
import smptk.image.Interpolation
import smptk.image.Resample
import java.io.File

object Test { 
  def main(args : Array[String])  { 
        val discreteImage = ImageIO.read2DScalarImage[Short](new File("/tmp/test.h5")).get
      println("image read")
        val continuousImage = Interpolation.interpolate2D(0)(discreteImage)
        println("interpolated")
      val tSpace = TranslationSpace2D()
      val translation = tSpace(DenseVector[Float](10,0))

        val translatedImg = continuousImage.warp(translation)
     println("warped")
      val resampledImage = Resample.sample2D[Short](translatedImg, discreteImage.domain, 0)      
      println("resampled")
      ImageIO.writeImage(resampledImage, new File("/tmp/resampled.h5"))
      println("written")
  }
}
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