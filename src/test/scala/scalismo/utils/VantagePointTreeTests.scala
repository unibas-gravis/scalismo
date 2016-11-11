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
package scalismo.utils

import scalismo.ScalismoTestSuite
import scalismo.geometry.{ Point, _3D }

import scala.collection.immutable.IndexedSeq

class VantagePointTreeTests extends ScalismoTestSuite {

  // seeded random generator
  implicit val rnd = Random(42)

  def randomPoint()(implicit rnd: Random): Point[_3D] = Point(rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble(), rnd.scalaRandom.nextDouble())

  // test size, VP size, lookup size
  val n = 50
  val nQuery = 10
  val points = {
    var s = IndexedSeq.fill(n)(randomPoint)
    while (s.distinct.length < s.length) s = IndexedSeq.fill(n)(randomPoint)
    s
  }
  val metric = Metric[Point[_3D]]((p, q) => (p - q).norm)

  describe("A VantagePointTree") {
    // build the tree
    val t = VantagePointTree(points, metric, VantagePointTree.randomPivotSelector[Point[_3D]])

    it("linearizes to points of construction") {
      val tseq = t.toIndexedSeq
      tseq should contain theSameElementsAs points
    }

    it("contains all points of construction") {
      points.foreach { p =>
        t.contains(p) shouldBe true
      }
    }

    it("does not contain points which it does not have") {
      var p = randomPoint()
      var i = 0 // timeout
      while (points.contains(p) && i < 10) {
        p = randomPoint()
        i += 1
      }
      if (i == 10) fail("could not find points which are not in the tree!")
      t.contains(p) shouldBe false
    }

    it("is a set") {
      val seq = t.toIndexedSeq
      seq.distinct should contain theSameElementsAs seq
    }

    it("finds all own points as closest points to themselves") {
      val closest = points.map(p => t.findClosestPoint(p))
      closest shouldBe points
    }

    it("finds the correct closest points using a few random points as query") {
      val rpoints = IndexedSeq.fill(nQuery)(randomPoint)
      val vpClosest = rpoints.map(p => t.findClosestPoint(p))
      // find closest points in list, linear, known result, as reference
      val listClosest = rpoints.map(p => points.minBy(metric(_, p)))
      vpClosest should contain theSameElementsAs listClosest
    }

    it("finds the k nearest neighbours to random points") {
      val k = 6
      val rpoints = IndexedSeq.fill(nQuery)(randomPoint)
      val vpClosest = rpoints.map(p => t.findKNearestNeighbours(p, k))
      val listClosest = rpoints.map(p => points.sortBy(metric(_, p)).take(k))
      vpClosest should contain theSameElementsAs listClosest
    }

    it("finds all neighbours within an epsilon region around random points") {
      val eps = 0.2
      val rpoints = IndexedSeq.fill(nQuery)(randomPoint)
      val vpClosest = rpoints.map(p => t.findEpsilonNeighbours(p, eps))
      val listClosest = rpoints.map(p => points.zip(points.map(metric(_, p))).sortBy(_._2).takeWhile(_._2 <= eps).map(_._1))
      vpClosest.zip(listClosest).foreach {
        case (vpLookUp, linLookUp) => vpLookUp should contain theSameElementsAs linLookUp
      }
    }

    it("finding 1 nearest neighbour is equivalent to findClosestPoint") {
      val k = 1
      val rpoints = IndexedSeq.fill(nQuery)(randomPoint)
      val kClosest = rpoints.map(p => t.findKNearestNeighbours(p, k)).map(_.head)
      val closest = rpoints.map(p => t.findClosestPoint(p))
      kClosest should contain theSameElementsAs closest
    }
  }
}
