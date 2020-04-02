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

import breeze.linalg.DenseMatrix
import scalismo.common.UnstructuredPointsDomain.Create.{CreateUnstructuredPointsDomain1D, CreateUnstructuredPointsDomain2D, CreateUnstructuredPointsDomain3D}
import scalismo.common.{DiscreteDomain, PointId, UnstructuredPoints, UnstructuredPointsDomain}
import scalismo.geometry.{Point, _1D, _2D, _3D}
import scalismo.mesh.{LineCell, LineList, LineMesh, LineMesh2D, LineMesh3D, TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D, TriangleCell, TriangleList, TriangleMesh, TriangleMesh2D, TriangleMesh3D}

import scala.language.higherKinds


trait StatismoDomainIO[D, DDomain[D] <: DiscreteDomain[D]] {

  def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[D]]): DDomain[D]

  def cellsToArray(mesh: DDomain[D]): NDArray[Int]
}


object StatismoDomainIO {

  implicit object domainIOTriangleMesh2D extends StatismoDomainIO[_2D, TriangleMesh] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_2D]]): TriangleMesh[_2D] = {
      val cells = for (i <- 0 until cellMatrix.cols)
        yield TriangleCell(PointId(cellMatrix(0, i)), PointId(cellMatrix(1, i)), PointId(cellMatrix(2, i)))
      TriangleMesh2D(UnstructuredPoints(points), TriangleList(cells))
    }

    override def cellsToArray(mesh: TriangleMesh[_2D]): NDArray[Int] = {
      val triangles = mesh.triangulation.triangles
      val cellArray = triangles.map(_.ptId1.id) ++ triangles.map(_.ptId2.id) ++ triangles.map(_.ptId3.id)
      NDArray(IndexedSeq(3, triangles.size), cellArray.toArray)
    }
  }

  implicit object domainIOTriangleMesh3D extends StatismoDomainIO[_3D, TriangleMesh] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_3D]]): TriangleMesh[_3D] = {
      val cells = for (i <- 0 until cellMatrix.cols)
        yield TriangleCell(PointId(cellMatrix(0, i)), PointId(cellMatrix(1, i)), PointId(cellMatrix(2, i)))
      TriangleMesh3D(UnstructuredPoints(points), TriangleList(cells))
    }

    override def cellsToArray(mesh: TriangleMesh[_3D]): NDArray[Int] = {
      val triangles = mesh.triangulation.triangles
      val cellArray = triangles.map(_.ptId1.id) ++ triangles.map(_.ptId2.id) ++ triangles.map(_.ptId3.id)
      NDArray(IndexedSeq(3, triangles.size), cellArray.toArray)
    }
  }

  implicit object domainIOTetrahedralMesh3D extends StatismoDomainIO[_3D, TetrahedralMesh] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_3D]]): TetrahedralMesh[_3D] = {
      val cells = for (i <- 0 until cellMatrix.cols)
        yield TetrahedralCell(PointId(cellMatrix(0, i)), PointId(cellMatrix(1, i)), PointId(cellMatrix(2, i)), PointId(cellMatrix(3, i)))
      TetrahedralMesh3D(UnstructuredPoints(points), TetrahedralList(cells))
    }

    override def cellsToArray(mesh: TetrahedralMesh[_3D]): NDArray[Int] = {
      val tetrahedrons = mesh.tetrahedralization.tetrahedrons
      val cellArray = tetrahedrons.map(_.ptId1.id) ++ tetrahedrons.map(_.ptId2.id) ++ tetrahedrons.map(_.ptId3.id) ++ tetrahedrons.map(_.ptId4.id)
      NDArray(IndexedSeq(4, tetrahedrons.size), cellArray.toArray)
    }
  }

  implicit object domainIOLineMesh2D extends StatismoDomainIO[_2D, LineMesh] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_2D]]): LineMesh[_2D] = {
      val cells = for (i <- 0 until cellMatrix.cols)
        yield LineCell(PointId(cellMatrix(0, i)), PointId(cellMatrix(1, i)))
      LineMesh2D(UnstructuredPoints(points), LineList(cells))
    }

    override def cellsToArray(mesh: LineMesh[_2D]): NDArray[Int] = {
      val cellArray = mesh.cells.map(_.ptId1.id) ++ mesh.cells.map(_.ptId2.id)
      NDArray(IndexedSeq(2, mesh.cells.size), cellArray.toArray)
    }
  }

  implicit object domainIOLineMesh3D extends StatismoDomainIO[_3D, LineMesh] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_3D]]): LineMesh[_3D] = {
      val cells = for (i <- 0 until cellMatrix.cols)
        yield LineCell(PointId(cellMatrix(0, i)), PointId(cellMatrix(1, i)))
      LineMesh3D(UnstructuredPoints(points), LineList(cells))
    }

    override def cellsToArray(mesh: LineMesh[_3D]): NDArray[Int] = {
      val cellArray = mesh.cells.map(_.ptId1.id) ++ mesh.cells.map(_.ptId2.id)
      NDArray(IndexedSeq(2, mesh.cells.size), cellArray.toArray)
    }
  }


  implicit object domainIOUnstructuredPoints1D extends StatismoDomainIO[_1D, UnstructuredPointsDomain] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_1D]]): UnstructuredPointsDomain[_1D] = {
      CreateUnstructuredPointsDomain1D.create(points)
    }

    override def cellsToArray(mesh: UnstructuredPointsDomain[_1D]): NDArray[Int] = {
      NDArray(IndexedSeq(0, 0), Array.empty)
    }
  }

  implicit object domainIOUnstructuredPoints2D extends StatismoDomainIO[_2D, UnstructuredPointsDomain] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_2D]]): UnstructuredPointsDomain[_2D] = {
      CreateUnstructuredPointsDomain2D.create(points)
    }

    override def cellsToArray(mesh: UnstructuredPointsDomain[_2D]): NDArray[Int] = {
      NDArray(IndexedSeq(0, 0), Array.empty)
    }
  }

  implicit object domainIOUnstructuredPoints3D extends StatismoDomainIO[_3D, UnstructuredPointsDomain] {
    override def createDomainWithCells(cellMatrix: DenseMatrix[Int], points: IndexedSeq[Point[_3D]]): UnstructuredPointsDomain[_3D] = {
      CreateUnstructuredPointsDomain3D.create(points)
    }

    override def cellsToArray(mesh: UnstructuredPointsDomain[_3D]): NDArray[Int] = {
      NDArray(IndexedSeq(0, 0), Array.empty)
    }
  }


}