package scalismo.image

import scalismo.ScalismoTestSuite
import scalismo.common.{BoxDomain, PointWithId}
import scalismo.geometry.*

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

    it("finds the correct nearest neighbour") {
      val domain =
        DiscreteImageDomain3D(Point3D(1.0, 3.5, 42.0), EuclideanVector3D(1.0, 2.1, 0.42), IntVector3D(42, 49, 32))

      val query = Point3D(4.0, 20.0, 44.0)
      val cp = domain.structuredPoints.findClosestPoint(query)
      val brutForce = domain.structuredPoints.pointsWithId.map(pId => ((pId._1 - query).norm, pId)).minBy(_._1)

      val bf = PointWithId(brutForce._2._1, brutForce._2._2)

      cp shouldBe bf
    }

    it("finds the correct n nearest neighbours") {
      val domain =
        DiscreteImageDomain3D(Point3D(1.0, 3.5, 42.0), EuclideanVector3D(1.0, 2.1, 0.42), IntVector3D(42, 49, 32))

      val query = Point3D(4.0, 20.0, 44.0)
      val N = 28
      val cp = domain.structuredPoints.findNClosestPoints(query, N)

      val brutForce =
        domain.structuredPoints.pointsWithId.map(pId => ((pId._1 - query).norm, pId)).toSeq.sortBy(_._1).take(N)
      val bf = brutForce.map(bf => PointWithId(bf._2._1, bf._2._2))

      cp.size shouldBe bf.size
      bf.foreach(bf => cp.contains(bf) shouldBe true)
    }
  }

}
