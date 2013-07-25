package smptk.image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.geometry._
import smptk.geometry.implicits._
import breeze.linalg.DenseVector
import smptk.registration.TranslationSpace1D

class ImageTest extends FunSpec with ShouldMatchers {
  describe("A discrete 1D image") {
    it("returns the same points for a 1d index and a coordinate index") {
      val domain = DiscreteImageDomain1D(0., 1, 5)
      val discreteImage = DiscreteScalarImage1D(domain, Array(3., 2., 1.5, 1, 0))

      for (i <- 0 until domain.size(0)) {
        assert(discreteImage(i) == discreteImage(i))
      }
    }
  }

  describe("A discrete 2D image") {
    it("returns the same points for a 1d index and a (2d) coordinate index") {
      val domain = DiscreteImageDomain2D((0., 0.), (1., 2.), (3, 2))
      val discreteImage = DiscreteScalarImage2D(domain, Array(3., 2., 1.5, 1., 0., 4.))

      for (
        y <- 0 until domain.size(1);
        x <- 0 until domain.size(0)
      ) {
        assert(discreteImage(y * domain.size(0) + x) === discreteImage((x, y)))
      }
    }
  }

  describe("A continuous 1D image") {
    it("yields the right values after composing with a translation") {
      def isInside(x: Point[OneD]) = x(0) >= -4. && x(0) <= 6.
      val image = ContinuousScalarImage1D(isInside,
        (x: Point[OneD]) => Math.sin(x(0).toDouble).toFloat,
        Some((x: Point[OneD]) => DenseVector(Math.cos(x(0).toDouble).toFloat)))
      val translationTransform = TranslationSpace1D()(DenseVector(1f))
      val composedImage = image.compose(translationTransform)
      assert(composedImage.isDefinedAt(-4f) === true)
      assert(composedImage.isDefinedAt(5f) === true)      
      assert(composedImage.isDefinedAt(-4.5f) === false)
      assert(composedImage.isDefinedAt(5.5f) === false)
      composedImage(0) should be(image(1) plusOrMinus 1e-5f)
    }

    it("yields the right values after warping with a translation") {
      def fixedDomainIsInside(x: Point[OneD]) = x >= -3 && x <= 7
      def movingDomainIsInside(x: Point[OneD]) = x >= -4 && x <= 6

      val image = ContinuousScalarImage1D(movingDomainIsInside,
        (x: Point[OneD]) => Math.sin(x(0).toDouble).toFloat,
        Some((x: Point[OneD]) => DenseVector(Math.cos(x(0).toDouble).toFloat)))
      val translationTransform = TranslationSpace1D()(DenseVector(-1f))
      
      val warpedImage = image.backwardWarp(translationTransform, fixedDomainIsInside)
      warpedImage.isDefinedAt(-4f) should be(false)
      warpedImage.isDefinedAt(-3f) should be(true)      
      warpedImage.isDefinedAt(5f) should be(true)
      warpedImage.isDefinedAt(-3.5f) should be(false)
      warpedImage.isDefinedAt(5.5f) should be(true)
      warpedImage.isDefinedAt(6.5f) should be(true)      
      warpedImage.isDefinedAt(7f) should be(true)      
      warpedImage(0) should be(image(-1) plusOrMinus 1e-5f)
    }

  }

}

class DomainTest extends FunSpec with ShouldMatchers {
  describe("a domain") {
    it("correctly reports the number of points") {
      val domain = DiscreteImageDomain2D((0., 0.), (1., 2.), (42, 49))
      assert(domain.numberOfPoints === domain.points.size)
    }

  }

  describe("a discrete domain in 2d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain2D((0., 0.), (1., 2.), (42, 49))
      assert(domain.indexToLinearIndex((40, 34)) === 40 + 34 * domain.size(0))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain2D((1., 2.), (2., 1.), (42, 49))
      val idx: Index2D = (5, 7)
      val recIdx = domain.linearIndexToIndex(domain.indexToLinearIndex(idx))
      assert(recIdx === idx)
    }
  }

  describe("a discrete domain in 3d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain3D((0., 0., 0.), (1., 2., 3.), (42, 49, 65))
      assert(domain.indexToLinearIndex((40, 34, 15)) === 40 + 34 * domain.size(0) + 15 * domain.size(0) * domain.size(1))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain3D((0., 0., 0.), (1., 2., 3.), (42, 49, 65))
      val idx: Index3D = (5, 3, 7)
      val recIdx = domain.linearIndexToIndex(domain.indexToLinearIndex(idx))
      assert(recIdx === idx)
    }
  }

}