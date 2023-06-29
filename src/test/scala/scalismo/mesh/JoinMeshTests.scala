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

import java.io.File
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.Point
import scalismo.io.MeshIO

import scala.language.implicitConversions

class JoinMeshTests extends ScalismoTestSuite {

  describe("Meshoperations.join for triangle meshes") {

    object Fixture {
      implicit def toPointId(i: Int): PointId = PointId(i)

      val oneTrianglesMesh = TriangleMesh3D(IndexedSeq(Point(0, 0, 0), Point(0, 0, 1), Point(1, 0, 0), Point(0, 1, 1)),
                                            TriangleList(IndexedSeq(TriangleCell(0, 1, 2)))
      )

      val twoTrianglesMesh = TriangleMesh3D(IndexedSeq(Point(0, 0, 0), Point(0, 0, 1), Point(1, 0, 0), Point(0, 1, 1)),
                                            TriangleList(IndexedSeq(TriangleCell(0, 1, 2), TriangleCell(0, 2, 3)))
      )
    }

    it("yields consecutive point ids") {
      val mesh1t = Fixture.oneTrianglesMesh
      val mesh2t = Fixture.twoTrianglesMesh
      val joinedMesh = mesh1t.operations.join(mesh2t)
      val pointIds = joinedMesh.pointSet.pointIds.toIndexedSeq
      pointIds.map(
        _.id
      ) should contain theSameElementsInOrderAs (0 until (mesh1t.pointSet.numberOfPoints + mesh2t.pointSet.numberOfPoints))
    }

    it("yields a mesh whose surface area is approximately equal the sum of the individual meshes") {
      val mesh1t = Fixture.oneTrianglesMesh
      val mesh2t = Fixture.twoTrianglesMesh
      val joinedMesh = mesh1t.operations.join(mesh2t)
      joinedMesh.area should be(mesh1t.area + mesh2t.area +- 1e-5)
    }
  }

  describe("Meshoperations.join for tetrahedral meshes") {

    object Fixture {
      implicit def toPointId(i: Int): PointId = PointId(i)

      val oneTetrahedralMesh =
        TetrahedralMesh3D(IndexedSeq(Point(0, 0, 0), Point(0, 0, 1), Point(1, 0, 0), Point(0, 1, 1)),
                          TetrahedralList(IndexedSeq(TetrahedralCell(0, 1, 2, 3)))
        )

      val twoTetrahedralMesh = TetrahedralMesh3D(
        IndexedSeq(Point(0, 0, 0), Point(0, 0, 1), Point(1, 0, 0), Point(0, 1, 1), Point(1, 1, 1)),
        TetrahedralList(IndexedSeq(TetrahedralCell(0, 1, 2, 3), TetrahedralCell(1, 2, 3, 4)))
      )
    }

    it("yields consecutive point ids") {
      val mesh1t = Fixture.oneTetrahedralMesh
      val mesh2t = Fixture.twoTetrahedralMesh
      val joinedMesh = mesh1t.operations.join(mesh2t)
      val pointIds = joinedMesh.pointSet.pointIds.toIndexedSeq
      pointIds.map(
        _.id
      ) should contain theSameElementsInOrderAs (0 until (mesh1t.pointSet.numberOfPoints + mesh2t.pointSet.numberOfPoints))
    }

    it("yields a mesh whose surface area is approximately equal the sum of the individual meshes") {
      val mesh1t = Fixture.oneTetrahedralMesh
      val mesh2t = Fixture.twoTetrahedralMesh
      val joinedMesh = mesh1t.operations.join(mesh2t)
      joinedMesh.volume should be(mesh1t.volume + mesh2t.volume +- 1e-5)
    }
  }
}
