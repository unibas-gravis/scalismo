package scalismo.image

import scalismo.ScalismoTestSuite
import scalismo.common.BoxDomain
import scalismo.geometry.{_2D, EuclideanVector, EuclideanVector2D, IntVector2D, Point2D}

class DiscreteImageDomainTests extends ScalismoTestSuite {

  describe("A DiscreteImageDomain") {
    it("keeps the same bounding box when it is created with a new size") {
      val domain = DiscreteImageDomain2D(Point2D(1.0, 3.5), EuclideanVector2D(1.0, 2.1), IntVector2D(42, 49))
      val newDomain = DiscreteImageDomain2D(domain.boundingBox, size = domain.size.map(i => (i * 1.5).toInt))

      newDomain.boundingBox.origin should equal(domain.boundingBox.origin)
      newDomain.boundingBox.volume should be(domain.boundingBox.volume +- 1e-1)
    }

    it("keeps approximately the same bounding box when it is created with a new spacing") {
      val domain = DiscreteImageDomain2D(Point2D(1.0, 3.5), EuclideanVector2D(1.0, 2.1), IntVector2D(42, 49))
      val newDomain = DiscreteImageDomain2D(domain.boundingBox, spacing = domain.spacing.map(i => i * 1.5))

      newDomain.boundingBox.origin should equal(domain.boundingBox.origin)

      // as the size needs to be integer, it can be that the imageBox is slightly larger.
      // The difference is, however , guaranteed to be smaller than the spacing in each direction. This is also
      // the difference between bounding and image box.
      newDomain.boundingBox.volume should be >= domain.boundingBox.volume
      newDomain.boundingBox.volume should be <= BoxDomain(
        domain.boundingBox.origin,
        domain.boundingBox.oppositeCorner + EuclideanVector(1.0, 1.0)
      ).volume
    }
  }

}
