package smptk.image

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Interpolation._


class InterpolationTest extends FunSpec with ShouldMatchers {
  describe("A 1D Interpolation with 0th order bspline") {
    it("interpolates the values for origin 0 and spacing 1") {
      val domain = DiscreteImageDomain1D(0f, 1, Tuple1(5))
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(3f, 2f, 1.5f, 1f, 0f))
      val continuousImg = interpolate(0)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(idx))
      }
    }

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3f,1.5f, Tuple1(7))
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(1.4f, 2.1f, 1.5f, 9f, 8f, 0f, 2.1f))
      val continuousImg = interpolate(0)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(idx))
      }
    }
  }

  describe("A 1D Interpolation with 1th order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3f, 1.5f, Tuple1(7))
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(1.4f, 2.1f, 1.5f, 9f, 8f, 0f, 2.1f))
      val continuousImg = interpolate(1)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
    	  continuousImg(pt) should be (discreteImage(idx) plusOrMinus 0.0001f)
      }
    }

  }
}
