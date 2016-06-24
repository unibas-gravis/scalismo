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

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{ PointId, UnstructuredPointsDomain }
import scalismo.geometry.Point.implicits._
import scalismo.geometry.{ Point, _3D }
import scalismo.io.MeshIO
import scalismo.registration.{ RotationSpace, ScalingSpace }

import scala.language.implicitConversions

class MeshTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat
  implicit def intToPointId(i: Int): PointId = PointId(i)

  describe("a mesh") {
    val path = getClass.getResource("/facemesh.stl").getPath
    val facemesh = MeshIO.readMesh(new File(path)).get

    it("finds the right closest points for all the points that define the mesh") {

      for ((pt, id) <- facemesh.pointSet.points.zipWithIndex) {
        val ptWithID = facemesh.pointSet.findClosestPoint(pt)
        val closestPt = ptWithID.point
        val closestId = ptWithID.id
        assert(closestPt === pt)
        assert(closestId.id === id)
      }
    }
    it("finds the right closest point for a point that is not defined on the mesh") {
      val pts = IndexedSeq(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0), Point(1.0, 1.0, 5.0))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val mesh = TriangleMesh3D(UnstructuredPointsDomain(pts), TriangleList(cells))

      val newPt = Point(1.1, 1.1, 4)
      val ptWithID = mesh.pointSet.findClosestPoint(newPt)
      val closestPt = ptWithID.point
      val closestPtId = ptWithID.id
      assert(closestPtId.id === 2)
      assert(closestPt === pts(2))
    }
    it("computes its area correctly for a triangle") {
      val pts: IndexedSeq[Point[_3D]] = IndexedSeq((0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 0.0, 0.0))
      val cells = IndexedSeq(TriangleCell(0, 1, 2))
      val mesh = TriangleMesh3D(UnstructuredPointsDomain(pts), TriangleList(cells))

      val R = RotationSpace[_3D]((0.0, 0.0, 0.0)).transformForParameters(DenseVector(0.3, 0.4, 0.1))
      val s = ScalingSpace[_3D].transformForParameters(DenseVector(2.0))
      val transformedMesh = mesh.transform(R).transform(s)
      mesh.area should be(0.5 +- 1e-8)
      transformedMesh.area should be(4.0f * mesh.area +- 1e-5) // scaling by two gives 4 times the area 
    }

    it("computes the right binary image for the unit sphere") {
      val path = getClass.getResource("/unit-sphere.stl").getPath
      val spheremesh = MeshIO.readMesh(new File(path)).get
      val binaryImg = Mesh.meshToBinaryImage(spheremesh)
      binaryImg(Point(0, 0, 0)) should be(1)
      binaryImg(Point(2, 0, 0)) should be(0)
    }

    it("can have an empty cell list") {
      val pts = IndexedSeq(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0), Point(1.0, 1.0, 5.0))
      val cells = IndexedSeq[TriangleCell]()
      val mesh = TriangleMesh3D(UnstructuredPointsDomain(pts), TriangleList(cells)) // would throw exception on fail
    }
  }
}