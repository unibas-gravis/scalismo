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
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.common.BoxDomain
import scalismo.geometry.IntVector.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.EuclideanVector.implicits._
import scalismo.geometry._
import scalismo.io.ImageIO
import scalismo.utils.Random

class StructuredPointsTests extends ScalismoTestSuite {

  implicit val rng: Random = Random(42)

  describe("a structured points pointset") {
    it("correctly reports the number of points") {
      val domain = StructuredPoints[_2D]((0.0, 0.0), (1.0, 2.0), (42, 49))
      assert(domain.numberOfPoints === domain.points.size)
    }

    it("yields valid point ids") {
      val domain = StructuredPoints[_2D]((0.0, 0.0), (1.0, 1.0), (20, 20))
      for (pt <- domain.points) {
        val ptId = domain.pointId(pt).get
        ptId.id should be < domain.numberOfPoints
      }
    }

    it("identifies the closest point correctly") {
      val domain = StructuredPoints[_2D]((0.0, 0.0), (1.0, 1.0), (20, 20))
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
      val domain = StructuredPoints[_2D]((0.0, 0.0), (1.0, 2.0), (42, 49))
      assert(domain.pointId((40, 34)).id === 40 + 34 * domain.size(0))
    }

    it("can correctly map a linear index to an index and back") {
      val domain = StructuredPoints[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      val idx = IntVector(5, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("equality works for image domains ") {
      val domain1 = StructuredPoints[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      val domain2 = StructuredPoints[_2D]((1.0, 2.0), (2.0, 1.0), (42, 49))
      assert(domain1 == domain2)
    }

  }

  describe("a discreteImageDomain in 3d") {

    object Fixture {
      val pathH5 = getClass.getResource("/3dimage.nii").getPath
      val img = ImageIO.read3DScalarImage[Short](new File(URLDecoder.decode(pathH5, "UTF-8"))).get
    }
    it("correctly maps a coordinate index to a linearIndex") {
      val domain = StructuredPoints[_3D]((0.0, 0.0, 0.0), (1.0, 2.0, 3.0), (42, 49, 65))
      assert(domain.pointId((40, 34, 15)).id === 40 + 34 * domain.size(0) + 15 * domain.size(0) * domain.size(1))
    }

    it("can correctly map a linear index to an index and back") {
      val domain = StructuredPoints[_3D]((0.0, 0.0, 0.0), (1.0, 2.0, 3.0), (42, 49, 65))

      val idx = IntVector(5, 3, 7)
      val recIdx = domain.index(domain.pointId(idx))
      assert(recIdx === idx)
    }

    it("correctly computes closest points") {

      // this test implicitly tests also the method pointToContinuousIndex

      val domain = StructuredPoints[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 1.0), (42, 49, 74))

      val distx = rng.breezeRandBasis.uniform.map(u => domain.spacing(0) * 0.4)
      val disty = rng.breezeRandBasis.uniform.map(u => domain.spacing(1) * 0.4)
      val distz = rng.breezeRandBasis.uniform.map(u => domain.spacing(2) * 0.4)

      for (pt <- domain.points) {
        val perturbedPoint = Point(pt.x + distx.draw(), pt.y + disty.draw(), pt.z + distz.draw())
        domain.findClosestPoint(perturbedPoint).point should equal(pt)
      }
    }

    it("equality works for image domains ") {
      val domain1 = StructuredPoints[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 1.0), (42, 49, 74))
      val domain2 = StructuredPoints[_3D]((1.0, 2.0, 3.0), (2.0, 1.0, 1.0), (42, 49, 74))
      assert(domain1 == domain2)
    }

    it("the internal transformations are correct") {
      val img = Fixture.img
      val trans = img.domain.pointSet.indexToPoint _
      val inverseTrans = img.domain.pointSet.pointToIndex _

      assert((trans(IntVector3D(0, 0, 0)) - img.domain.origin).norm < 0.1)

      val upperRightMostPoint = IntVector3D(img.domain.size(0) - 1, img.domain.size(1) - 1, img.domain.size(2) - 1)
      (trans(upperRightMostPoint) - img.domain.pointSet.boundingBox.oppositeCorner).norm should be < 0.1

      val index = IntVector3D(3, 7, 1)
      inverseTrans(trans(index)) should equal(index)
    }

    it("Domain points in chunks returns the correct list of points") {
      val points = Fixture.img.domain.pointSet.points
      val chunkedPoints = Fixture.img.domain.pointSet.pointsInChunks(24)
      val concatenated = chunkedPoints.reduce(_ ++ _)
      points zip concatenated foreach { case (p1, p2) => assert(p1 == p2) }
    }

  }
}
