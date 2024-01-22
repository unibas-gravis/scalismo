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
import scalismo.io.MeshIO

import java.io.File
import java.net.URLDecoder
import scala.language.implicitConversions

class JoinMeshTests extends ScalismoTestSuite {

  describe("Meshoperations.join for triangle meshes") {

    object Fixture {
      implicit def toPointId(i: Int): PointId = PointId(i)

      private val points1 = IndexedSeq(Point(0, 0, 0), Point(0, 0, 1), Point(1, 0, 0), Point(0, 1, 1))
      private val triangels1 = TriangleList(IndexedSeq(TriangleCell(0, 1, 2)))
      val mesh1 = TriangleMesh3D(points1, triangels1)

      private val points2 = IndexedSeq(Point(10, 10, 10), Point(10, 10, 11), Point(11, 10, 10), Point(10, 11, 11))
      private val triangels2 = TriangleList(IndexedSeq(TriangleCell(0, 1, 2), TriangleCell(0, 2, 3)))
      val mesh2 = TriangleMesh3D(points2, triangels2)

      private val newPoints = points1 ++ points2
      private val newTriangles =
        TriangleList(IndexedSeq(TriangleCell(0, 1, 2), TriangleCell(4, 5, 6), TriangleCell(4, 6, 7)))
      val joinedMeshExpected = TriangleMesh3D(newPoints, newTriangles)
    }

    it("results in the correctly joined mesh") {
      val joinedMesh = Fixture.mesh1.operations.join(Fixture.mesh2)
      joinedMesh should equal(Fixture.joinedMeshExpected)
    }

  }

  describe("Meshoperations.join for tetrahedral meshes") {

    object Fixture {
      implicit def toPointId(i: Int): PointId = PointId(i)

      private val points1 = IndexedSeq(Point(0, 0, 0), Point(0, 0, 1), Point(1, 0, 0), Point(0, 1, 1))
      private val tetrahedra1 = TetrahedralList(IndexedSeq(TetrahedralCell(0, 1, 2, 3)))
      val mesh1 = TetrahedralMesh3D(points1, tetrahedra1)

      private val points2 = IndexedSeq(Point(10, 10, 10), Point(10, 10, 11), Point(11, 10, 10), Point(10, 11, 11))
      private val tetrahedra2 = TetrahedralList(IndexedSeq(TetrahedralCell(0, 1, 2, 3), TetrahedralCell(1, 2, 3, 4)))
      val mesh2 = TetrahedralMesh3D(points2, tetrahedra2)

      private val newPoints = points1 ++ points2
      private val newTetrahedra = TetrahedralList(
        IndexedSeq(TetrahedralCell(0, 1, 2, 3), TetrahedralCell(4, 5, 6, 7), TetrahedralCell(5, 6, 7, 8))
      )
      val joinedMeshExpected = TetrahedralMesh3D(newPoints, newTetrahedra)

    }

    it("results in the correctly joined mesh\"") {
      val joinedMesh = Fixture.mesh1.operations.join(Fixture.mesh2)
      joinedMesh should equal(Fixture.joinedMeshExpected)
    }
  }
}
