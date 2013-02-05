package smptk.image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Interpolation._
import org.scalatest.Ignore

class InterpolationTest extends FunSpec with ShouldMatchers {
  describe("A 1D Interpolation with 0rd order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3f, 1.5f, 7)
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 2.1f))
      val continuousImg = interpolate(0)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(idx))
      }
    }
  }

  describe("A 1D Interpolation with 1th order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3f, 1.5f, 7)
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 2.1f))
      val continuousImg = interpolate(1)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
      }
    }

    
    it("interpolates the values for origin 0 and spacing 1") {
    	val domain = DiscreteImageDomain1D(0f, 1, 5)
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(3f, 2f, 1.5f, 1f, 0f))
      val continuousImg = interpolate(0)(discreteImage)
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(idx))
      }
    }

    
  }

  describe("A 2D interpolation  Spline") {

    describe("of degree 0") {

      it("Has coefficients equal to the image samples") {
        val domain = DiscreteImageDomain2D((1f, 0f), (0.5f, 1f), (2, 3))
        val discreteImage = DiscreteScalarImage2D[Float](domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))
        for (idx <- 0 until discreteImage.domain.points.size) {
          val coeffs = determineCoefficients(0, discreteImage)
          coeffs(idx) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Interpolates the values for a simple domain") {
        val domain = DiscreteImageDomain2D((0f, 0f), (1f, 1f), (2, 3))
        val discreteImage = DiscreteScalarImage2D(domain, IndexedSeq(1f, 2f, 3f, 4f, 5f, 6f))

        val continuousImg = interpolate2D(0)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

      it("Interpolates the values for origin (2,3) and spacing (1.5, 2.3)") {
        val domain = DiscreteImageDomain2D((2f, 3f), (1.5f, 0.1f), (2, 3))
        val discreteImage = DiscreteScalarImage2D(domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate2D(0)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }
    describe(" of degree 3") {
      it("Interpolates the values for origin (2,3) and spacing (1.5, 2.3)") {
        val domain = DiscreteImageDomain2D((2f, 3f), (1.5f, 1.3f), (2, 3))
        val discreteImage = DiscreteScalarImage2D(domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate2D(3)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }
  }
  describe("A 3D interpolation  Spline") {
    describe("of degree 0") {

      it("Has coefficients equal to the image samples") {
    	val domain = DiscreteImageDomain3D((2f, 3f, 0f), (1.5f, 1.3f, 2.0f), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        for (idx <- 0 until discreteImage.domain.points.size) {
          val coeffs = determineCoefficients(0, discreteImage)
          coeffs(idx) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }
      
      
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2f, 3f, 0f), (1.5f, 1.3f, 2.0f), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate3D(0)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }
      
    }
    
    describe(" of degree 1") {
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2f, 3f, 0f), (1.5f, 1.3f, 2.0f), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate3D(1)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }
    
    describe(" of degree 3") {
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2f, 3f, 0f), (1.5f, 1.3f, 2.0f), (2, 3, 2))
        val discreteImage = DiscreteScalarImage3D[Float](domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = interpolate3D(3)(discreteImage)

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(idx) plusOrMinus 0.0001f)
        }
      }

    }

  }
}
