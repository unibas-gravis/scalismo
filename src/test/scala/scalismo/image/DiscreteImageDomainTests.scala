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
import scalismo.geometry.Index.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.Vector.implicits._
import scalismo.geometry._
import scalismo.io.ImageIO

class DiscreteImageDomainTests extends ScalismoTestSuite {

  describe("a discreteImageDomain domain") {
    it("correctly reports the number of points") {
      val domain = DiscreteImageDomain[_2D]((0.0f, 0.0f), (1.0f, 2.0f), (42, 49))
      assert(domain.numberOfPoints === domain.points.size)
    }

    it("yields valid point ids") {
      val domain = DiscreteImageDomain[_2D]((0f, 0f), (1.0f, 1.0f), (20, 20))
      for (pt <- domain.points) {
        val ptId = domain.pointId(pt).get
        ptId should be < domain.numberOfPoints
      }
    }

    it("keeps the same boundingbox when it is create with a new size") {
      val domain = DiscreteImageDomain[_2D]((1.0f, 3.5f), (1.0f, 2.1f), (42, 49))
      val newDomain = DiscreteImageDomain(domain.boundingBox, size = domain.size.map(i => (i * 1.5f).toInt))

      newDomain.boundingBox.origin should equal(domain.boundingBox.origin)
      newDomain.boundingBox.volume should be(domain.boundingBox.volume +- 1e-1f)
    }

    it("keeps the same boundingbox approximately the same when it is create with a new spacing") {
      val domain = DiscreteImageDomain[_2D]((1.0f, 3.5f), (1.0f, 2.1f), (42, 49))
      val newDomain = DiscreteImageDomain(domain.boundingBox, spacing = domain.spacing.map(i => (i * 1.5f)))

      newDomain.boundingBox.origin should equal(domain.boundingBox.origin)

      // as the size needs to be integer, it can be that the imageBox is slightly larger.
      // The difference is, however , guaranteed to be smaller than the spacing in each direction. This is also
      // the difference between bounding and image box.
      newDomain.boundingBox.volume should be >= (domain.boundingBox.volume)
      newDomain.boundingBox.volume should be <= BoxDomain(domain.boundingBox.origin, domain.boundingBox.oppositeCorner + Vector(1f, 1f)).volume
    }

    it("identifies the closest point correctly") {
      val domain = DiscreteImageDomain[_2D]((0f, 0f), (1.0f, 1.0f), (20, 20))
      def testPoint(pt: Point[_2D], correctClosestPoint: Point[_2D]) = {
        val (closestPt, closestPtId) = domain.findClosestPoint(pt)
        closestPt should equal(correctClosestPoint)
        closestPtId should equal(domain.pointId(closestPt).get)
        closestPtId should be < domain.numberOfPoints
      }

      // test all points that are inside the domain
      for (pt <- domain.points) {
        testPoint(pt, pt)
      }

      // test a point that is outside of the domain
      testPoint(Point(0.1f, 20.6f), Point(0f, 19f))
    }

  }

  describe("a discreteImageDomain  in 2d") {
    val domain1 = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))
    val domain2 = DiscreteImageDomain[_2D]((1.0f, 2.0f), (2.0f, 1.0f), (42, 49))

    it("correctly maps a coordinate index to a linearIndex") {
      val domain1 = DiscreteImageDomain[_2D]((0.0f, 0.0f), (1.0f, 2.0f), (42, 49))
      assert(domain1.pointId((40, 34)) === 40 + 34 * domain1.size(0))
    }

    it("can correclty map a linear index to an index and back") {
      val idx = Index(5, 7)
      val recIdx = domain1.index(domain1.pointId(idx))
      assert(recIdx === idx)
    }

    it("domains with same parameters yield to the same anisotropic simlarity transform ") {
      assert(domain1.indexToPhysicalCoordinateTransform == domain2.indexToPhysicalCoordinateTransform)
    }

    it("equality works for image domains ") {
      assert(domain1 == domain2)
    }

    it("has 4 neighbors at an inner point") {
      val innerPtId = domain1.numberOfPoints / 2
      domain1.neighbors(innerPtId).size == 4
    }

    it("has 2 neighbors at the origin and oppositeCorner") {
      domain1.neighbors(0) == 2
      val indexTopRightCorner = domain1.size.map(v => v - 1)
      val pointIdTopRightCorner = domain1.pointId(indexTopRightCorner)
      domain1.neighbors(pointIdTopRightCorner) == 2
    }

  }

  describe("a discreteImageDomain in 3d") {

    val domain1 = DiscreteImageDomain[_3D]((1.0f, 2.0f, 3f), (2.0f, 1.0f, 1f), (42, 49, 74))
    val domain2 = DiscreteImageDomain[_3D]((1.0f, 2.0f, 3f), (2.0f, 1.0f, 1f), (42, 49, 74))

    it("correctly maps a coordinate index to a linearIndex") {
      val domain = DiscreteImageDomain[_3D]((0.0f, 0.0f, 0.0f), (1.0f, 2.0f, 3.0f), (42, 49, 65))
      assert(domain.pointId((40, 34, 15)) === 40 + 34 * domain.size(0) + 15 * domain.size(0) * domain.size(1))
    }

    it("can correclty map a linear index to an index and back") {
      val domain = DiscreteImageDomain[_3D]((0.0f, 0.0f, 0.0f), (1.0f, 2.0f, 3.0f), (42, 49, 65))

      val idx = Index(5, 3, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("domains with same parameters yield to the same anisotropic simlarity transform ") {
      assert(domain1.indexToPhysicalCoordinateTransform == domain2.indexToPhysicalCoordinateTransform)
    }
    it("equality works for image domains ") {
      assert(domain1 == domain2)
    }

    it("the anisotropic similarity transform defining the donmain is correct and invertible") {
      val pathH5 = getClass.getResource("/3dimage.nii").getPath
      val origImg = ImageIO.read3DScalarImage[Short](new File(pathH5)).get

      val trans = origImg.domain.indexToPhysicalCoordinateTransform
      val inverseTrans = trans.inverse

      assert((trans(Point(0, 0, 0)) - origImg.domain.origin).norm < 0.1f)
      assert(inverseTrans(origImg.domain.origin).toVector.norm < 0.1f)

      (trans(Point(origImg.domain.size(0) - 1, origImg.domain.size(1) - 1, origImg.domain.size(2) - 1)) - origImg.domain.boundingBox.oppositeCorner).norm should be < (0.1)
      (inverseTrans(origImg.domain.boundingBox.oppositeCorner) - Point(origImg.domain.size(0) - 1, origImg.domain.size(1) - 1, origImg.domain.size(2) - 1)).norm should be < (0.1)
    }

    it("has 6 neighbors at an inner point") {
      val innerPtId = domain1.numberOfPoints / 2
      domain1.neighbors(innerPtId).size == 6
    }

    it("has 3 neighbors at the origin and oppositeCorner") {
      domain1.neighbors(0) == 3
      val indexTopRightCorner = domain1.size.map(v => v - 1)
      val pointIdTopRightCorner = domain1.pointId(indexTopRightCorner)
      domain1.neighbors(pointIdTopRightCorner) == 3
    }

  }
}
