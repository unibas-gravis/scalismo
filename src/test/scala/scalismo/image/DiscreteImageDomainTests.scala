/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalismo.image

import java.io.File

import scalismo.ScalismoTestSuite
import scalismo.common.BoxDomain
import scalismo.geometry.IntVector.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.EuclideanVector.implicits._
import scalismo.geometry._
import scalismo.io.ImageIO

class DiscreteImageDomainTests extends ScalismoTestSuite {

  describe("a discreteImageDomain domain") {
    it("correctly reports the number of points") {
      val domain = DiscreteImageDomain[_2D]((0.0, 0.0), (1.0, 2.0), (42, 49))
      assert(domain.numberOfPoints === domain.points.size)
    }

    it("yields valid point ids") {
      val domain = DiscreteImageDomain[_2D]((0.0, 0.0), (1.0, 1.0), (20, 20))
      for (pt <- domain.points) {
        val ptId = domain.pointId(pt).get
        ptId.id should be < domain.numberOfPoints
      }
    }

    it("keeps the same bounding box when it is created with a new size") {
      val domain = DiscreteImageDomain[_2D]((1.0, 3.5), (1.0, 2.1), (42, 49))
      val newDomain = DiscreteImageDomain(domain.boundingBox, size = domain.size.map(i => (i * 1.5).toInt))

      newDomain.boundingBox.origin should equal(domain.boundingBox.origin)
      newDomain.boundingBox.volume should be(domain.boundingBox.volume +- 1e-1)
    }

    it("keeps approximately the same bounding box when it is created with a new spacing") {
      val domain = DiscreteImageDomain[_2D]((1.0, 3.5), (1.0, 2.1), (42, 49))
      val newDomain = DiscreteImageDomain(domain.boundingBox, spacing = domain.spacing.map(i => i * 1.5))

      newDomain.boundingBox.origin should equal(domain.boundingBox.origin)

      // as the size needs to be integer, it can be that the imageBox is slightly larger.
      // The difference is, however , guaranteed to be smaller than the spacing in each direction. This is also
      // the difference between bounding and image box.
      newDomain.boundingBox.volume should be >= domain.boundingBox.volume
      newDomain.boundingBox.volume should be <= BoxDomain(domain.boundingBox.origin, domain.boundingBox.oppositeCorner + EuclideanVector(1.0, 1.0)).volume
    }

    it("identifies the closest point correctly") {
      val domain = DiscreteImageDomain[_2D]((0.0, 0.0), (1.0, 1.0), (20, 20))
      def testPoint(pt: Point[_2D], correctClosestPoint: Point[_2D]) = {
        val ptWithId = domain.findClosestPoint(pt)
        val closestPt = ptWithId.point
        val closestPtId = ptWithId.id
        closestPt should equal(correctClosestPoint)
        closestPtId should equal(domain.pointId(closestPt).get)
        closestPtId.id should be < domain.numberOfPoints
      }

      // test all points that are inside the domain
      for (pt <- domain.points) {
        testPoint(pt, pt)
      }

      // test a point that is outside of the domain
      testPoint(Point(0.1, 20.6), Point(0.0, 19.0))
    }

  }

  describe("a discreteImageDomain  in 2d") {
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain[_2D]((0.0, 0.0), (1.0, 2.0), (42, 49))
      assert(domain.pointId((40, 34)).id === 40 + 34 * domain.size(0))
    }

    it("can correctly map a linear index to an index and back") {
      val domain = DiscreteImageDomain[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      val idx = IntVector(5, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("domains with same parameters yield the same anisotropic similarity transform ") {
      val domain1 = DiscreteImageDomain[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      val domain2 = DiscreteImageDomain[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      assert(domain1.indexToPhysicalCoordinateTransform == domain2.indexToPhysicalCoordinateTransform)
    }
    it("equality works for image domains ") {
      val domain1 = DiscreteImageDomain[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      val domain2 = DiscreteImageDomain[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      assert(domain1 == domain2)
    }

  }

  describe("a discreteImageDomain in 3d") {

    object Fixture {
      val pathH5 = getClass.getResource("/3dimage.nii").getPath
      val img = ImageIO.read3DScalarImage[Short](new File(pathH5)).get
    }
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain[_3D]((0.0, 0.0, 0.0), (1.0, 2.0, 3.0), (42, 49, 65))
      assert(domain.pointId((40, 34, 15)).id === 40 + 34 * domain.size(0) + 15 * domain.size(0) * domain.size(1))
    }

    it("can correctly map a linear index to an index and back") {
      val domain = DiscreteImageDomain[_3D]((0.0, 0.0, 0.0), (1.0, 2.0, 3.0), (42, 49, 65))

      val idx = IntVector(5, 3, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("domains with same parameters yield the same anisotropic similarity transform ") {
      val domain1 = DiscreteImageDomain[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 0.0), (42, 49, 74))
      val domain2 = DiscreteImageDomain[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 0.0), (42, 49, 74))
      assert(domain1.indexToPhysicalCoordinateTransform == domain2.indexToPhysicalCoordinateTransform)
    }
    it("equality works for image domains ") {
      val domain1 = DiscreteImageDomain[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 1.0), (42, 49, 74))
      val domain2 = DiscreteImageDomain[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 1.0), (42, 49, 74))
      assert(domain1 == domain2)
    }

    it("the anisotropic similarity transform defining the domain is correct and invertible") {

      val img = Fixture.img

      val trans = img.domain.indexToPhysicalCoordinateTransform
      val inverseTrans = trans.inverse

      assert((trans(Point(0, 0, 0)) - img.domain.origin).norm < 0.1)
      assert(inverseTrans(img.domain.origin).toVector.norm < 0.1)

      (trans(Point(img.domain.size(0) - 1, img.domain.size(1) - 1, img.domain.size(2) - 1)) - img.domain.boundingBox.oppositeCorner).norm should be < 0.1
      (inverseTrans(img.domain.boundingBox.oppositeCorner) - Point(img.domain.size(0) - 1, img.domain.size(1) - 1, img.domain.size(2) - 1)).norm should be < 0.1
    }

    it("Domain points in chunks returns the correct list of points") {
      val points = Fixture.img.domain.points
      val chunkedPoints = Fixture.img.domain.pointsInChunks(24)
      val concatenated = chunkedPoints.reduce(_ ++ _)
      points zip concatenated foreach { case (p1, p2) => assert(p1 == p2) }
    }

  }
}
