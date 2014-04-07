//package org.statismo.stk.core.image
//
//import org.scalatest.FunSpec
//import org.scalatest.matchers.ShouldMatchers
//import org.statismo.stk.core.image.Interpolation._
//import org.scalatest.Ignore
//import org.statismo.stk.core.geometry._
//import org.statismo.stk.core.geometry.implicits._
//import breeze.plot.Figure
//import breeze.plot._
//import org.scalatest.PrivateMethodTester
//import breeze.linalg.DenseVector
//import org.statismo.stk.core.io.ImageIO
//import java.io.File
//import org.statismo.stk.core.numerics.Integrator
//import org.statismo.stk.core.numerics.IntegratorConfiguration
//import org.statismo.stk.core.numerics.UniformSampler1D
//import org.statismo.stk.core.numerics.UniformSampler2D
//import org.statismo.stk.core.common.BoxedDomain1D
//import org.statismo.stk.core.common.BoxedDomain1D
//import org.statismo.stk.core.common.RealSpace1D
//import org.statismo.stk.core.common.BoxedDomain2D
//import org.statismo.stk.core.utils.Visualization._
//import org.statismo.stk.core.common.BoxedDomain
//
//class SmoothingTest extends FunSpec with ShouldMatchers with PrivateMethodTester {
//
//  org.statismo.stk.core.initialize
//
//  describe("A 1D function smoothing") {
//
//    val domain = DiscreteImageDomain1D(-100., 0.1, 2000)
//   
//    def noisySine(x: Point[OneD]): Float = {
//      //continuousImg(x)
//      (Math.sin(x(0)) + breeze.stats.distributions.Gaussian(0., 0.4).draw).toFloat
//
//    }
//
//    val noisyImage = ContinuousScalarImage1D(BoxedDomain1D(-5.0, 5.0), noisySine)
//    //val discreteDomain = DiscreteImageDomain1D(-5., 0.1, 100)
//    val box = BoxedDomain1D(-5.0, 5.0)
//
//    ignore("Works with a box filter (via convolution)") {
//      show(noisyImage, box, -1.0)
//      val boxFilter = BoxedFilter1D(1)
//      val smoothed = noisyImage.convolve(boxFilter,100)
//      show(smoothed, box, -1.0)
//    }
//
//    ignore("Works with Gaussian filter") {
//      val gaussianFilter = GaussianFilter1D(1)
//      val gaussianSmoothed = noisyImage.convolve(gaussianFilter,100)
//      show(gaussianSmoothed, box, -1.0)
//
//    }
//
//  }
//
//  describe("A 1D convolution ") {
//
//    ignore("gives the b-spline basis function when convolving a box with a box filter") {
//
//      val boxImage = ContinuousScalarImage1D(RealSpace1D, (p: Point[OneD]) => if (p(0) >= -0.5 && p(0) <= 0.5) 1f else 0f)
//      val filter = BoxedFilter1D(1)
//
//      val discreteDomain = DiscreteImageDomain1D(Point1D(-5), Vector1D(0.01f), Index1D(1000))
//
//      val convoledOnce = boxImage.convolve(filter, 100)
//      val convolvedTwice = convoledOnce.convolve(filter,100)
//      val cv2 = Resample.sample[Float](convolvedTwice, discreteDomain, -1f)
//      val convolvedThreeTimes = Interpolation.interpolate(cv2, 3).convolve(filter,100)
//     
//      val box = BoxedDomain1D(-5, 5)
//      show(Resample.sample[Float](convoledOnce, discreteDomain, -1f))
//      show(cv2)
//      show(Resample.sample[Float](convolvedThreeTimes, discreteDomain, -1f))
//
//      convoledOnce(0f) should be(Interpolation.bSpline(1)(0.).toFloat plusOrMinus (0.1f))
//      convolvedTwice(0f) should be(Interpolation.bSpline(2)(0.).toFloat plusOrMinus (0.1f))
//      convolvedThreeTimes(0f) should be(Interpolation.bSpline(3)(0.).toFloat plusOrMinus (0.1f))
//
//    }
//
//  }
//
//  describe("A 2D smoothing") {
//
//    ignore("works for a box image ") {
//
//      def domainBox(origin: Point[TwoD], extent: Point[TwoD]) =
//        (p: Point[TwoD]) => (p(0) >= origin(0) && p(1) >= origin(1) && p(0) <= extent(0) && p(1) <= extent(1))
//
//      def innerBox(origin: Point2D, extent: Point2D) = (p: Point[TwoD]) => if (domainBox(origin, extent)(p)) 200f else 1f
//
//      val boxImage = ContinuousScalarImage2D(BoxedDomain2D((-5., -5.), (5., 5.)), innerBox((-2., -2.), (2., 2.)))
//
//      val box = BoxedDomain2D(Point2D(-6f, -6f), Point2D(6f, 6f))
//
//      show(boxImage, box, -1f)
//
//      val filter = GaussianFilter2D(1.)
//      val smoothed = boxImage.convolve(filter, 400)
//      show(smoothed, box, -1f)
//
//    }
//
//    val discreteImage = ImageIO.read2DScalarImage[Short](new File(getClass.getResource("/lena256.h5").getPath())).get
//    
//    println("Lena's domain : " + discreteImage.domain)
//    val originalImage = Interpolation.interpolate(discreteImage, 3)
//    val f = (p:Point[TwoD]) => if(p.toVector.norm < 5) 250f else originalImage(p)
//    val dummyImage = ContinuousScalarImage2D(discreteImage.domain, f,  None)
//    show(dummyImage, BoxedDomain2D(discreteImage.domain.origin, discreteImage.domain.extent), -1f)
//
//    it("works with box filter") {
//      val boxFilter = GaussianFilter2D(6.)
//
//      val smoothed = originalImage.convolve(boxFilter,400)
//      show(smoothed, BoxedDomain2D(discreteImage.domain.origin, discreteImage.domain.extent), -1f)
//    }
//
//    ignore("works with Gaussian filter") {
//
//      val deviations = List(8., 6., 4., 2., 1.)
//
//      for (d <- deviations) {
//        val smoothed = originalImage.convolve(GaussianFilter2D(d), 100)
//        show(smoothed, BoxedDomain2D(discreteImage.domain.origin, discreteImage.domain.extent), -1f)
//      }
//    }
//
//  }
//
//}
