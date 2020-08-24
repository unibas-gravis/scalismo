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
package scalismo.io

import scalismo.ScalismoTestSuite
import scalismo.geometry._
import scalismo.common.{PointId}
import scalismo.common.UnstructuredPoints.Create.{CreateUnstructuredPoints2D, CreateUnstructuredPoints3D}
import scalismo.mesh.{
  LineCell,
  LineList,
  LineMesh2D,
  LineMesh3D,
  TetrahedralCell,
  TetrahedralList,
  TetrahedralMesh3D,
  TriangleCell,
  TriangleList,
  TriangleMesh2D,
  TriangleMesh3D
}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class StatismoDomainIOTests extends ScalismoTestSuite {

  describe("a Point Distribution Model IO helper") {

    it("can convert a 2D TriangleMesh") {
      val unstructuredPoints =
        CreateUnstructuredPoints2D.create(IndexedSeq(Point2D(0, 0), Point2D(1, 0), Point2D(1, 1)))
      val topology = TriangleList(IndexedSeq(TriangleCell(PointId(0), PointId(1), PointId(2))))
      val input = TriangleMesh2D(unstructuredPoints, topology)
      val cellsAsArray: NDArray[Int] = StatismoDomainIO.domainIOTriangleMesh2D.cellsToArray(input)
      val t = for {
        output <- StatismoDomainIO.domainIOTriangleMesh2D.createDomainWithCells(unstructuredPoints.points.toIndexedSeq,
                                                                                Option(cellsAsArray))
      } yield {
        assert(input == output)
      }
      t.get
    }

    it("can convert a 3D TriangleMesh") {
      val unstructuredPoints =
        CreateUnstructuredPoints3D.create(IndexedSeq(Point3D(0, 0, 0), Point3D(1, 0, 0), Point3D(1, 1, 0)))
      val topology = TriangleList(IndexedSeq(TriangleCell(PointId(0), PointId(1), PointId(2))))
      val input = TriangleMesh3D(unstructuredPoints, topology)
      val cellsAsArray: NDArray[Int] = StatismoDomainIO.domainIOTriangleMesh3D.cellsToArray(input)
      val t = for {
        output <- StatismoDomainIO.domainIOTriangleMesh3D.createDomainWithCells(unstructuredPoints.points.toIndexedSeq,
                                                                                Option(cellsAsArray))
      } yield {
        assert(input == output)
      }
      t.get
    }

    it("can convert a 3D TetrahedralMesh") {
      val unstructuredPoints = CreateUnstructuredPoints3D.create(
        IndexedSeq(Point3D(0, 0, 0), Point3D(1, 0, 0), Point3D(1, 1, 0), Point3D(1, 1, 1))
      )
      val topology = TetrahedralList(IndexedSeq(TetrahedralCell(PointId(0), PointId(1), PointId(2), PointId(3))))
      val input = TetrahedralMesh3D(unstructuredPoints, topology)
      val cellsAsArray: NDArray[Int] = StatismoDomainIO.domainIOTetrahedralMesh3D.cellsToArray(input)
      val t = for {
        output <- StatismoDomainIO.domainIOTetrahedralMesh3D
          .createDomainWithCells(unstructuredPoints.points.toIndexedSeq, Option(cellsAsArray))
      } yield {
        assert(input == output)
      }
      t.get
    }

    it("can convert a 2D LineMesh") {
      val unstructuredPoints = CreateUnstructuredPoints2D.create(IndexedSeq(Point2D(0, 0), Point2D(1, 0)))
      val topology = LineList(IndexedSeq(LineCell(PointId(0), PointId(1))))
      val input = LineMesh2D(unstructuredPoints, topology)
      val cellsAsArray: NDArray[Int] = StatismoDomainIO.domainIOLineMesh2D.cellsToArray(input)
      val t = for {
        output <- StatismoDomainIO.domainIOLineMesh2D.createDomainWithCells(unstructuredPoints.points.toIndexedSeq,
                                                                            Option(cellsAsArray))
      } yield {
        assert(input == output)
      }
      t.get
    }

    it("can convert a 3D LineMesh") {
      val unstructuredPoints =
        CreateUnstructuredPoints3D.create(IndexedSeq(Point3D(0, 0, 0), Point3D(1, 0, 0), Point3D(1, 1, 0)))
      val topology = LineList(IndexedSeq(LineCell(PointId(0), PointId(1))))
      val input = LineMesh3D(unstructuredPoints, topology)
      val cellsAsArray: NDArray[Int] = StatismoDomainIO.domainIOLineMesh3D.cellsToArray(input)
      val t = for {
        output <- StatismoDomainIO.domainIOLineMesh3D.createDomainWithCells(unstructuredPoints.points.toIndexedSeq,
                                                                            Option(cellsAsArray))
      } yield {
        assert(input == output)
      }
      t.get
    }
  }
}
