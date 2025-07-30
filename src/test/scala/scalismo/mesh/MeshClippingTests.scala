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

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{PointId, UnstructuredPoints}
import scalismo.geometry.Point.implicits.*
import scalismo.geometry.{Point, Point3D, _3D}
import scalismo.io.MeshIO
import scalismo.transformations.*

import java.io.File
import java.net.URLDecoder
import scala.language.implicitConversions

class MeshClippingTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat
  implicit def intToPointId(i: Int): PointId = PointId(i)

  def lowestStart(cell: TriangleCell): TriangleCell = {
    val startIdx = cell.pointIds.map(_.id).zipWithIndex.minBy(_._1)._2
    startIdx match {
      case 0 => cell
      case 1 => TriangleCell(cell.ptId2, cell.ptId3, cell.ptId1)
      case 2 => TriangleCell(cell.ptId3, cell.ptId1, cell.ptId2)
    }
  }

  describe("a mesh clipping operation") {

    object Fixture {
      val points = IndexedSeq(Point3D(0, 0, 0), Point3D(1, 0, 0), Point3D(0, 1, 0), Point3D(0, 0, 1))
      val cells = TriangleList(IndexedSeq(TriangleCell(PointId(0), PointId(1), PointId(2)), TriangleCell(PointId(0), PointId(2), PointId(3))))
      val mesh = TriangleMesh3D(points, cells)
      val meshWithFreePoint = TriangleMesh3D(points, TriangleList(cells.triangles.drop(1)))
      val meshWithFreePointAtEnd = TriangleMesh3D(points, TriangleList(cells.triangles.dropRight(1)))
    }

    it("should clip a mesh correctly") {
      val mesh = Fixture.mesh

      val clipped = mesh.operations.clip(_.z > 0.5)

      clipped.pointSet.numberOfPoints shouldBe 3
      clipped.pointSet.pointSequence shouldBe Fixture.points.dropRight(1)
      clipped.triangulation.triangles shouldBe Fixture.cells.triangles.dropRight(1)
    }

    it("should clip a mesh correctly also with points not contained in a triangle") {
      {
        val mesh = Fixture.meshWithFreePoint

        val clipped = mesh.operations.clip(_.z > 0.5)

        val clippedPoints = clipped.pointSet.pointSequence
        val expectedPoints = Fixture.points.dropRight(1)

        clippedPoints.size shouldBe 3
        expectedPoints.foreach(pt => clippedPoints.contains(pt) shouldBe true)
        clipped.triangulation.triangles shouldBe IndexedSeq()
      }
      {
        val mesh = Fixture.meshWithFreePointAtEnd

        val clipped = mesh.operations.clip(_.z > 0.5)

        val clippedPoints = clipped.pointSet.pointSequence
        val expectedPoints = Fixture.points.dropRight(1)

        clippedPoints.size shouldBe 3
        expectedPoints.foreach(pt => clippedPoints.contains(pt) shouldBe true)
        clipped.triangulation.triangles.map(lowestStart) shouldBe mesh.triangulation.triangles.map(lowestStart)
      }
    }
  }
}
