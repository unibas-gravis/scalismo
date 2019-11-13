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
package scalismo.mesh

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.Point
import scala.language.implicitConversions

class MeshBoundaryTests extends ScalismoTestSuite {

  describe("a mesh boundary") {

    object Fixture {
      implicit def toPointId(i: Int) = PointId(i)

      val singleTriangleMesh = TriangleMesh3D(IndexedSeq(
        Point(0, 0, 0),
        Point(0, 0, 1),
        Point(1, 0, 0)
      ),
        TriangleList(IndexedSeq(
          TriangleCell(0, 1, 2)
        ))
      )

      val twoTraingesMesh = TriangleMesh3D(IndexedSeq(
        Point(0, 0, 0),
        Point(0, 0, 1),
        Point(1, 0, 0),
        Point(0, 1, 1)
      ),
        TriangleList(IndexedSeq(
          TriangleCell(0, 1, 2),
          TriangleCell(0, 2, 3)
        ))
      )

      val traingesMeshWithOneCompletelySouroundedTriangle = {
        val points = for (y <- 0 until 4; x <- 0 until 4) yield Point(x, y, 0)
        val trianglesV = for (y <- 0 until 3; x <- 0 until 3) yield TriangleCell(x + y * 4, (x + 1) + y * 4, x + (y + 1) * 4)
        val trianglesA = for (y <- 0 until 3; x <- 0 until 3) yield TriangleCell(x + 1 + y * 4, x + (y + 1) * 4, (x + 1) + (y + 1) * 4)
        TriangleMesh3D(
          points,
          TriangleList(
            trianglesV ++ trianglesA
          )
        )
      }
    }

    it("should have all elements on the boundary for a single-triangle mesh") {
      val mesh = Fixture.singleTriangleMesh
      val b = MeshBoundaryPredicates(mesh)

      b.pointIsOnBoundary(PointId(0)) shouldBe true
      b.pointIsOnBoundary(PointId(1)) shouldBe true
      b.pointIsOnBoundary(PointId(2)) shouldBe true

      b.edgeIsOnBoundary(PointId(0), PointId(1)) shouldBe true
      b.edgeIsOnBoundary(PointId(1), PointId(2)) shouldBe true
      b.edgeIsOnBoundary(PointId(2), PointId(0)) shouldBe true
      b.edgeIsOnBoundary(PointId(1), PointId(0)) shouldBe true
      b.edgeIsOnBoundary(PointId(2), PointId(1)) shouldBe true
      b.edgeIsOnBoundary(PointId(0), PointId(2)) shouldBe true

      b.triangleIsOnBoundary(TriangleId(0)) shouldBe true
    }

    it("the edge between two triangles should not be on the boundary") {
      val mesh = Fixture.twoTraingesMesh
      val b = MeshBoundaryPredicates(mesh)

      b.edgeIsOnBoundary(PointId(0), PointId(2)) shouldBe false
    }

    it("edges not existing in the mesh should not be on the boundary") {
      val mesh = Fixture.twoTraingesMesh
      val b = MeshBoundaryPredicates(mesh)

      b.edgeIsOnBoundary(PointId(1), PointId(3)) shouldBe false
    }

    it("all elements of internal triangles should not be on the boundary") {
      val mesh = Fixture.traingesMeshWithOneCompletelySouroundedTriangle
      val b = MeshBoundaryPredicates(mesh)
      /*
      ----------
      |0/|1/|2/|
      |/9|/.|/.|
      ----------
      |3/|4/|5/|
      |/.|/.|/.|
      ----------
      |6/|7/|8/|
      |/.|/.|/.|
      ----------
       */

      def testTriangle(id: TriangleId): Unit = {
        val t4 = mesh.triangles(id.id)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe false
        b.pointIsOnBoundary(p1) shouldBe false
        b.pointIsOnBoundary(p2) shouldBe false

        b.edgeIsOnBoundary(p0, p1) shouldBe false
        b.edgeIsOnBoundary(p1, p2) shouldBe false
        b.edgeIsOnBoundary(p2, p0) shouldBe false
        b.edgeIsOnBoundary(p1, p0) shouldBe false
        b.edgeIsOnBoundary(p2, p1) shouldBe false
        b.edgeIsOnBoundary(p0, p2) shouldBe false

        b.triangleIsOnBoundary(id) shouldBe false
      }

      testTriangle(TriangleId(4))
      testTriangle(TriangleId(13))

    }

    it("test bonery for all elements of some border triangles") {
      val mesh = Fixture.traingesMeshWithOneCompletelySouroundedTriangle
      val b = MeshBoundaryPredicates(mesh)
      /*
      ----------
      |0/|1/|2/|
      |/9|/.|/.|      10 / 11
      ----------
      |3/|4/|5/|
      |/.|/.|/.| 12 / 13 / 14
      ----------
      |6/|7/|8/|
      |/.|/.|/.| 15 / 16 / 17
      ----------
      */

      testTriangle0()
      testTriangle1()
      testTriangle7()
      testTriangle9()
      testTriangle10()
      testTriangle16()
      testTriangle17()

      def testTriangle17(): Unit = {
        val t4 = mesh.triangles(17)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe true
        b.pointIsOnBoundary(p1) shouldBe true
        b.pointIsOnBoundary(p2) shouldBe true

        b.edgeIsOnBoundary(p0, p1) shouldBe false
        b.edgeIsOnBoundary(p1, p2) shouldBe true
        b.edgeIsOnBoundary(p2, p0) shouldBe true
        b.edgeIsOnBoundary(p1, p0) shouldBe false
        b.edgeIsOnBoundary(p2, p1) shouldBe true
        b.edgeIsOnBoundary(p0, p2) shouldBe true

        b.triangleIsOnBoundary(TriangleId(0)) shouldBe true
      }

      def testTriangle16(): Unit = {
        val id = 16
        val t4 = mesh.triangles(id)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe false
        b.pointIsOnBoundary(p1) shouldBe true
        b.pointIsOnBoundary(p2) shouldBe true

        b.edgeIsOnBoundary(p0, p1) shouldBe false
        b.edgeIsOnBoundary(p1, p2) shouldBe true
        b.edgeIsOnBoundary(p2, p0) shouldBe false
        b.edgeIsOnBoundary(p1, p0) shouldBe false
        b.edgeIsOnBoundary(p2, p1) shouldBe true
        b.edgeIsOnBoundary(p0, p2) shouldBe false

        b.triangleIsOnBoundary(TriangleId(id)) shouldBe true
      }

      def testTriangle10(): Unit = {
        val id = 10
        val t4 = mesh.triangles(id)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe true
        b.pointIsOnBoundary(p1) shouldBe false
        b.pointIsOnBoundary(p2) shouldBe false

        b.edgeIsOnBoundary(p0, p1) shouldBe false
        b.edgeIsOnBoundary(p1, p2) shouldBe false
        b.edgeIsOnBoundary(p2, p0) shouldBe false
        b.edgeIsOnBoundary(p1, p0) shouldBe false
        b.edgeIsOnBoundary(p2, p1) shouldBe false
        b.edgeIsOnBoundary(p0, p2) shouldBe false

        b.triangleIsOnBoundary(TriangleId(id)) shouldBe false
      }

      def testTriangle9(): Unit = {
        val id = 9
        val t4 = mesh.triangles(id)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe true
        b.pointIsOnBoundary(p1) shouldBe true
        b.pointIsOnBoundary(p2) shouldBe false

        b.edgeIsOnBoundary(p0, p1) shouldBe false
        b.edgeIsOnBoundary(p1, p2) shouldBe false
        b.edgeIsOnBoundary(p2, p0) shouldBe false
        b.edgeIsOnBoundary(p1, p0) shouldBe false
        b.edgeIsOnBoundary(p2, p1) shouldBe false
        b.edgeIsOnBoundary(p0, p2) shouldBe false

        b.triangleIsOnBoundary(TriangleId(id)) shouldBe false
      }

      def testTriangle7(): Unit = {
        val id = 7
        val t4 = mesh.triangles(id)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe false
        b.pointIsOnBoundary(p1) shouldBe false
        b.pointIsOnBoundary(p2) shouldBe true

        b.edgeIsOnBoundary(p0, p1) shouldBe false
        b.edgeIsOnBoundary(p1, p2) shouldBe false
        b.edgeIsOnBoundary(p2, p0) shouldBe false
        b.edgeIsOnBoundary(p1, p0) shouldBe false
        b.edgeIsOnBoundary(p2, p1) shouldBe false
        b.edgeIsOnBoundary(p0, p2) shouldBe false

        b.triangleIsOnBoundary(TriangleId(id)) shouldBe false
      }

      def testTriangle1(): Unit = {
        val id = 1
        val t4 = mesh.triangles(1)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe true
        b.pointIsOnBoundary(p1) shouldBe true
        b.pointIsOnBoundary(p2) shouldBe false

        b.edgeIsOnBoundary(p0, p1) shouldBe true
        b.edgeIsOnBoundary(p1, p2) shouldBe false
        b.edgeIsOnBoundary(p2, p0) shouldBe false
        b.edgeIsOnBoundary(p1, p0) shouldBe true
        b.edgeIsOnBoundary(p2, p1) shouldBe false
        b.edgeIsOnBoundary(p0, p2) shouldBe false

        b.triangleIsOnBoundary(TriangleId(id)) shouldBe true
      }

      def testTriangle0(): Unit = {
        val id = 0
        val t4 = mesh.triangles(id)
        val p0 = t4.ptId1
        val p1 = t4.ptId2
        val p2 = t4.ptId3

        b.pointIsOnBoundary(p0) shouldBe true
        b.pointIsOnBoundary(p1) shouldBe true
        b.pointIsOnBoundary(p2) shouldBe true

        b.edgeIsOnBoundary(p0, p1) shouldBe true
        b.edgeIsOnBoundary(p1, p2) shouldBe false
        b.edgeIsOnBoundary(p2, p0) shouldBe true
        b.edgeIsOnBoundary(p1, p0) shouldBe true
        b.edgeIsOnBoundary(p2, p1) shouldBe false
        b.edgeIsOnBoundary(p0, p2) shouldBe true

        b.triangleIsOnBoundary(TriangleId(id)) shouldBe true
      }
    }
  }
}