package smptk.image

import Image._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.CoordVector3D
import smptk.image.Geometry.CoordVector2D

class ImageTest extends FunSpec with ShouldMatchers {
  describe("A discrete 1D image") {
    it("returns the same points for a 1d index and a coordinate index") {
      val domain = DiscreteImageDomain1D(0f, 1, 5)
      val discreteImage = DiscreteScalarImage1D(domain, IndexedSeq(3f, 2f, 1.5f, 1f, 0f))

      for (i <- 0 until domain.size(0)) {
        assert(discreteImage(i) == discreteImage(CoordVector1D[Int](i)))
      }
    }
  }

  describe("A discrete 2D image") {
    it("returns the same points for a 1d index and a (2d) coordinate index") {
      val domain = DiscreteImageDomain2D((0f, 0f), (1f, 2f), (3, 2))
      val discreteImage = DiscreteScalarImage2D(domain, IndexedSeq(3f, 2f, 1.5f, 1f, 0f, 4f))

      for (
        y <- 0 until domain.size(1);
        x <- 0 until domain.size(0)
      ) {
        assert(discreteImage(y * domain.size(0) + x) === discreteImage((x, y)))
      }
    }
  }

}

class DomainTest extends FunSpec with ShouldMatchers {
  describe("a discrete domain in 2d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain2D((0f, 0f), (1f, 2f), (42, 49))
      assert(domain.indexToLinearIndex((40, 34)) === 40 + 34 * domain.size(0))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain2D((1f, 2f), (2f, 1f), (42, 49))
      val idx: CoordVector2D[Int] = (5, 7)
      val recIdx = domain.linearIndexToIndex(domain.indexToLinearIndex(idx))
      assert(recIdx === idx)
    }
  }

  describe("a discrete domain in 3d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain3D((0f, 0f, 0f), (1f, 2f, 3f), (42, 49, 65))
      assert(domain.indexToLinearIndex((40, 34, 15)) === 40 + 34 * domain.size(0) + 15 * domain.size(0) * domain.size(1))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain3D((0f, 0f, 0f), (1f, 2f, 3f), (42, 49, 65))
      val idx: CoordVector3D[Int] = (5, 3, 7)
      val recIdx = domain.linearIndexToIndex(domain.indexToLinearIndex(idx))
      assert(recIdx === idx)
    }
  }

}