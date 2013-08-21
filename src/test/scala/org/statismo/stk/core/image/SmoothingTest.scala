//package smptk.image
//
//import Image._
//import org.scalatest.FunSpec
//import org.scalatest.matchers.ShouldMatchers
//import smptk.image.Interpolation._
//import org.scalatest.Ignore
//import smptk.geometry._
//import smptk.geometry.implicits._
//import breeze.plot.Figure
//import breeze.plot._
//import org.scalatest.PrivateMethodTester
//import breeze.linalg.DenseVector
//import smptk.io.ImageIO
//import java.io.File
//import smptk.numerics.Integrator
//import smptk.numerics.IntegratorConfiguration
//import smptk.numerics.UniformSampler1D
//import smptk.numerics.UniformSampler2D
//import smptk.common.BoxedDomain1D
//import smptk.common.BoxedDomain1D
//import smptk.common.RealSpace1D
//import smptk.common.BoxedDomain2D
//
//// A lot of the tests below are meant to be evaluated visually and are therefore ignored
//
//class SmoothingTest extends FunSpec with ShouldMatchers with PrivateMethodTester {
//
//  describe("A 1D function smoothing") {
//
//    val distr = breeze.stats.distributions.Gaussian(0., 0.2)
//    val domain = DiscreteImageDomain1D(-100., 0.1, 2000)
//    val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => if (math.round(x(0)) % 10 == 0) -1. else 1.).toIndexedSeq)
//    val continuousImg = Interpolation.interpolate(discreteImage, 0)
//
//    def noisySine(x: Point[OneD]): Double = {
//      //continuousImg(x)
//      Math.sin(x(0)) + breeze.stats.distributions.Gaussian(0., 0.1).draw
//
//    }
//
//    val noisyImage = ContinuousScalarImage1D(BoxedDomain1D(-5.0, 5.0), noisySine)
//    val discreteDomain = DiscreteImageDomain1D(-5., 0.1, 100)
//    
//  
//    ignore("Works with a box filter (via convolution)") {
//      //Utils.show1D(noisyImage, discreteDomain)
//      val boxFilter = BoxedFilter1D()
//      val boxedregion = BoxedDomain1D(-0.5, 0.5)
//      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(boxedregion), 100))
//
//      val smoothed = noisyImage.convolve(boxFilter, integrator)
//      //Utils.show1D(smoothed, discreteDomain)
//    }
//
//    it("Works with Gaussian filter (via Utils method)") {
//
//      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(boxedregion), 5000))
//      val gaussianSmoothed = Utils.gaussianSmoothing1D(noisyImage, 0.01, integrator)
//      //Utils.show1D(gaussianSmoothed, discreteDomain)
//
//      val gaussianSmoothed2 = Utils.gaussianSmoothing1D(noisyImage, 2., integrator)
//      //Utils.show1D(gaussianSmoothed2, discreteDomain)
//
//      val p = Point1D(0.5)
//      gaussianSmoothed(p) should be(Math.sin(p(0)) plusOrMinus 0.01)
//    }
//
//  }
//
//  describe("A 1D convolution ") {
//
//    it("gives the b-spline basis function when convolving a box with a box filter") {
//
//      val boxImage = ContinuousScalarImage1D(RealSpace1D, (p: Point[OneD]) => if (p(0) >= -0.5 && p(0) <= 0.5) 1. else 0.)
//      val filter = BoxedFilter1D()
//
//      val integrator = Integrator[OneD](IntegratorConfiguration(UniformSampler1D(), 100))
//
//      val convoledOnce = boxImage.convolve(filter, integrator)
//      val convolvedTwice = convoledOnce.convolve(filter, integrator)
//      val convolvedThreeTimes = convolvedTwice.convolve(filter, integrator)
//
//      val domain = DiscreteImageDomain1D(-5., 0.1, 100)
//      //Utils.show1D(convoledOnce, domain)
//
//      convoledOnce(0.) should be(Interpolation.bSpline(1)(0.) plusOrMinus (0.1))
//      convolvedTwice(0.) should be(Interpolation.bSpline(2)(0.) plusOrMinus (0.1))
//      convolvedThreeTimes(0.) should be(Interpolation.bSpline(3)(0.) plusOrMinus (0.1))
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
//      def innerBox(origin: Point2D, extent: Point2D) = (p: Point[TwoD]) => if (domainBox(origin, extent)(p)) 2. else 1.
//
//      val boxImage = ContinuousScalarImage2D(BoxedDomain2D((-5., -5.), (5., 5.)), innerBox((-2., -2.), (3., 2.)))
//
//      val domain = DiscreteImageDomain2D((-6., -6.), (0.1, 0.1), (120, 120))
//
//      //Utils.show2D(boxImage, domain)
//
//      val integrator = Integrator[TwoD](IntegratorConfiguration(UniformSampler2D(), 5000))
//      val filter = GaussianFilter2D(5.)
//      val smoothed = boxImage.convolve(filter, integrator)
//
//      //Utils.show2D(smoothed, domain)
//
//    }
//
//    val discreteImage = ImageIO.read2DScalarImage[Short](new File(getClass.getResource("/lena256.h5").getPath())).get
//    val originalImage = Interpolation.interpolate(discreteImage, 3)
//    //Utils.show2D(originalImage, discreteImage.domain)
//
//    ignore("works with box filter") {
//      val boxThresh = 50.
//      val boxFilter = BoxedFilter2D()
//
//      val filterSupport = DiscreteImageDomain2D((-1., -1.), (0.1, 0.1), (100, 100))
//
//      val integrator = Integrator[TwoD](IntegratorConfiguration(UniformSampler2D(), 100))
//      val smoothed = originalImage.convolve(boxFilter, integrator)
//      //Utils.show2D(smoothed, discreteImage.domain)
//    }
//
//    ignore("works with Gaussian filter") {
//
//      val integrator = Integrator[TwoD](IntegratorConfiguration(UniformSampler2D(), 500))
//      val deviations = List(8., 6., 4., 2., 1.)
//
//      for (d <- deviations) {
//        val smoothed = Utils.gaussianSmoothing2D(originalImage, d, integrator)
//       // Utils.show2D(smoothed, discreteImage.domain)
//      }
//    }
//
//  }
//
//}
