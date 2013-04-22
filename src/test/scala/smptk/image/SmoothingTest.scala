package smptk.image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Interpolation._
import org.scalatest.Ignore
import smptk.image.Geometry._
import smptk.image.Geometry.implicits._
import breeze.plot.Figure
import breeze.plot._
import org.scalatest.PrivateMethodTester
import breeze.linalg.DenseVector
import smptk.io.ImageIO
import java.io.File
import smptk.numerics.UniformIntegrator
import smptk.numerics.UniformIntegratorConfiguration

class SmoothingTest extends FunSpec with ShouldMatchers with PrivateMethodTester {

  describe("A 1D function smoothing") {

    val distr = breeze.stats.distributions.Gaussian(0., 0.2)
    def noisySine(x: Point1D): Double = {
      Math.sin(x(0)) + distr.draw
    }

    val noisyImage = ContinuousScalarImage1D((p: Point1D) => true, noisySine)
    val discreteDomain = DiscreteImageDomain1D(CoordVector1D(-3.), CoordVector1D(0.1), 60)
    Utils.show1D(noisyImage, discreteDomain)

    it("Works with a box filter (via convolution)") {

      val boxFilter = (p: Point1D) => if (p(0) > (-0.5) || p(0) < 0.5) 20 else 0.01
      val filterSupport = DiscreteImageDomain1D(CoordVector1D(-1.), CoordVector1D(0.1), 20)
      val integrator = UniformIntegrator[CoordVector1D]()

      val smoothed = noisyImage.convolve(boxFilter, filterSupport, integrator)
      Utils.show1D(smoothed, discreteDomain)

    }

    it("Works with Gaussian filter (via Utils method)") {

      val integrator = UniformIntegrator[CoordVector1D](UniformIntegratorConfiguration(100))
      val gaussianSmoothed = Utils.gaussianSmoothing1D(noisyImage, 0.3, integrator)
      Utils.show1D(gaussianSmoothed, discreteDomain)

    }

  }

  describe("A 2D smoothing") {

    val discreteImage = ImageIO.read2DScalarImage[Short](new File(getClass.getResource("/lena256.h5").getPath())).get
    val originalImage = Interpolation.interpolate2D(3)(discreteImage)
    Utils.show2D(originalImage, discreteImage.domain)

    it("works with box filter") {
      val boxThresh = 50.
      val boxFilter = (p: Point2D) => if (p(0) > (-boxThresh) || p(0) < boxThresh || p(1) > (-boxThresh) || p(1) < boxThresh) 1 else 0.01

      val filterSupport = DiscreteImageDomain2D(CoordVector2D(-1., -1.), CoordVector2D(0.1, 0.1), CoordVector2D(100, 100))

      val integrator = UniformIntegrator[CoordVector2D](UniformIntegratorConfiguration(100))
      val smoothed = originalImage.convolve(boxFilter, filterSupport, integrator)
      Utils.show2D(smoothed, discreteImage.domain)
    }

    it("works with Gaussian filter") {

      val integrator = UniformIntegrator[CoordVector2D](UniformIntegratorConfiguration(100))
      val smoothed = Utils.gaussianSmoothing2D(originalImage, 1, integrator)

      Utils.show2D(smoothed, discreteImage.domain)
    }

  }

}
