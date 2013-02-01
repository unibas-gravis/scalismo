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
import smptk.image.ContinuousScalarImageLike

object Test {



// a concrete example of a registration
	def doRegistration(): RegistrationResult[CoordVector1D] = {

    val oneDTransformSpace = new TransformationSpace[CoordVector1D] {
      def apply(p: ParameterVector) = {
        new Transformation[CoordVector1D] { def apply(pt: Point1D) = p(0) * pt(0) }
      }
      def parametersDimensionality: Int = 1
      def takeDerivative(p: ParameterVector) = { x: Point1D =>
        val M = DenseMatrix.zeros[Float](1, 1)
        M(0, 0) = x(0)
        M
      }
    }

    val metric = new ImageMetric[CoordVector1D] {
    	def apply(img1: ContinuousScalarImageLike[CoordVector1D],
        img2: ContinuousScalarImageLike[CoordVector1D]) = {
    		(img1 - img2).squaredNorm
      }
      def takeDerivativeWRTToMovingImage(img1: ContinuousScalarImageLike[CoordVector1D],
        img2: ContinuousScalarImageLike[CoordVector1D]) = {
        (img1 - img2) * 2f
      }
    }

    val domain = ContinuousImageDomain1D(-5f, 10f)    
    val fixedImage  = ContinuousScalarImage1D(domain, x => x(0) * x(0), x => DenseVector(2 * x(0)))
    val movingImage = ContinuousScalarImage1D(domain, x => 3 * x(0) * x(0), x => DenseVector(6 * x(0)))

    val initialParameters = DenseVector(1f)
    val regularizer = new Regularizer {
      def apply(p: ParameterVector) = 0f
    }

    val regResult = Registration.registration(fixedImage, movingImage, oneDTransformSpace, metric, regularizer, initialParameters)

    val f = Figure()
    val p = f.subplot(0)
    val x = fixedImage.domain.uniformSamples(100).map(_.toFloat)
    
    p += plot(x, x.map(f => fixedImage(f)))
    val warpedimg = movingImage compose regResult.transform
    p += plot(x, x.map(f => warpedimg(f)), '+')
//    val alpha : DenseVector[Float]= linspace(-1, 1).map(_.toFloat)
//    def warpedImage(alpha : DenseVector[Float]) =[Space] { 
//      def apply(x : Point1D) = movingImage(oneDTransformSpace(alpha)(x))
//      def takeDerivative(x : Space#Point) = DenseVector(0)
//      def domain = fixedImage.domain
//    }
//    def metricValue(alpha : Float) = metric(fixedImage, warpedImage(DenseVector(alpha))) 
//    p += plot(alpha,  alpha.map(metricValue))
//    
//    val img = new ContinuousVectorImage[Space] {
//      val pixelDimensionality = 2
//      val domain = Simple1DDomain(-1, DenseVector(2f))
//      def apply(x: Space#Point) = DenseVector(x * x, x* x)
//      def takeDerivative(x: Space#Point) = DenseVector(0)
//    }
    regResult
	}


  def main(args: Array[String]) {
    val regResult = doRegistration()
    println("optimal parameters: " + regResult.parameters.toString)
    
  }

}