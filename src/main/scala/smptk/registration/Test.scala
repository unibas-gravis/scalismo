package smptk.registration

import scala.collection.immutable.{ Vector => SVector }
import TransformationSpace.ParameterVector
import Registration.RegistrationResult

import breeze.plot._
import breeze.linalg._

object Test {

//  type Space = Space1D
//
//  case class Simple1DDomain(val origin: Float, val extent: DenseVector[Float]) extends ImageDomain[Space1D]() {
//    def dimensionality = 1
//    def isInside(pt: Space1D#Point) = pt > origin && pt < origin + extent(0)
//    def uniformSamples(n: Int) = {
//      val spacing: Float = extent(0) / n
//      (0 until n) map (i => (origin + i * spacing))
//    }
//  }
//
//  // a concrete example of a registration
//  def doRegistration(): RegistrationResult[Space] = {
//
//    val oneDTransformSpace = new TransformationSpace[Space] {
//      def apply(p: ParameterVector) = {
//        new Transformation[Space] { def apply(pt: Space#Point) = p(0) * pt }
//      }
//      def parametersDimensionality: Int = 1
//      def takeDerivative(p: ParameterVector) = { x: Space#Point =>
//        val M = DenseMatrix.zeros[Float](1, 1)
//        M(0, 0) = x
//        M
//      }
//
//    }
//
//    val metric = new ImageMetric[Space] {
//      def apply(img1: ContinuousScalarImage[Space],
//        img2: ContinuousScalarImage[Space]) = {
//        (img1 - img2).squaredNorm
//
//      }
//      def takeDerivativeWRTToMovingImage(img1: ContinuousScalarImage[Space],
//        img2: ContinuousScalarImage[Space]) = {
//        (img1 - img2) * 2f
//      }
//    }
//
//    val fixedImage = new ContinuousScalarImage[Space] {
//      val domain = Simple1DDomain(-5, DenseVector(10f))
//      def apply(x: Space#Point) = x * x
//      def takeDerivative(x: Space#Point) = DenseVector(2 * x)
//
//    }
//
//    val movingImage = new ContinuousScalarImage[Space] {
//      val domain = Simple1DDomain(-5, DenseVector(10f))
//      def apply(x: Space#Point) = 3 * x * x
//      def takeDerivative(x: Space#Point) = DenseVector(6 * x)
//
//    }
//
//    val initialParameters = DenseVector(1f) // the 0 vector
//    val regularizer = new Regularizer {
//      def apply(p: ParameterVector) = 0f
//    }
//
//    val regResult = Registration.registration(fixedImage, movingImage, oneDTransformSpace, metric, regularizer, initialParameters)
//
//    val f = Figure()
//    val p = f.subplot(0)
//    val x = fixedImage.domain.uniformSamples(100)
//    
//    //p += plot(x, x.map(fixedImage))
//    //p += plot(x, x.map(movingImage compose regResult.transform), '+')
//    val alpha : DenseVector[Float]= linspace(-1, 1).map(_.toFloat)
//    def warpedImage(alpha : DenseVector[Float]) = new ContinuousScalarImage[Space] { 
//      def apply(x : Space#Point) = movingImage(oneDTransformSpace(alpha)(x))
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
//    regResult
//  }

  def main(args: Array[String]) {
    //val regResult = doRegistration()
    //println("optimal parameters: " + regResult.parameters.toString)
    println("hello world")
  }

}