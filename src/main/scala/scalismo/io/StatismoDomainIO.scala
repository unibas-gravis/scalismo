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

import java.io.File

import breeze.linalg.DenseMatrix
import scalismo.common.UnstructuredPointsDomain.Create.{CreateUnstructuredPointsDomain1D, CreateUnstructuredPointsDomain2D, CreateUnstructuredPointsDomain3D}
import scalismo.common.{DiscreteDomain, PointId, UnstructuredPoints, UnstructuredPointsDomain}
import scalismo.geometry.{Point, _1D, _2D, _3D}
import scalismo.mesh.{LineCell, LineList, LineMesh, LineMesh2D, LineMesh3D, TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D, TriangleCell, TriangleList, TriangleMesh, TriangleMesh2D, TriangleMesh3D}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


/**
 * IO handling of PointDistributionModels with different point connectivity
 *
 * @tparam D       : Domain dimensionality
 * @tparam DDomain : DiscretePointDomain
 */
trait StatismoDomainIO[D, DDomain[D] <: DiscreteDomain[D]] {

  val datasetType: String

  def createDomainWithCells(points: IndexedSeq[Point[D]], cellArray: Option[NDArray[Int]]): Try[DDomain[D]]

  def cellsToArray(mesh: DDomain[D]): NDArray[Int]
}

object StatismoDomainIO {

  private def ndIntArrayToIntMatrix(array: Option[NDArray[Int]]): DenseMatrix[Int] = {
    // the data in ndarray is stored row-major, but DenseMatrix stores it column major.
    // We therefore do switch dimensions.
    val a = array.get
    DenseMatrix.create(a.dims(1).toInt, a.dims(0).toInt, a.data)
  }


  implicit object domainIOTriangleMesh2D extends StatismoDomainIO[_2D, TriangleMesh] {
    override val datasetType: String = "POLYGON_MESH"

    override def createDomainWithCells(points: IndexedSeq[Point[_2D]], cellArray: Option[NDArray[Int]]): Try[TriangleMesh[_2D]] = {
      val cellMatrix = ndIntArrayToIntMatrix(cellArray)
      if(cellMatrix.cols != 3) Failure(new Exception("the representer cells are not triangles"))
      else {
        val cells = for (i <- 0 until cellMatrix.rows) yield {
          TriangleCell(PointId(cellMatrix(i, 0)), PointId(cellMatrix(i, 1)), PointId(cellMatrix(i, 2)))
        }
        Success(TriangleMesh2D(UnstructuredPoints(points), TriangleList(cells)))
      }
    }

    override def cellsToArray(mesh: TriangleMesh[_2D]): NDArray[Int] = {
      val triangles = mesh.triangulation.triangles
      val cellArray = triangles.map(_.ptId1.id) ++ triangles.map(_.ptId2.id) ++ triangles.map(_.ptId3.id)
      NDArray(IndexedSeq(3, triangles.size), cellArray.toArray)
    }
  }

  implicit object domainIOTriangleMesh3D extends StatismoDomainIO[_3D, TriangleMesh] {
    override val datasetType: String = "POLYGON_MESH"

    override def createDomainWithCells(points: IndexedSeq[Point[_3D]], cellArray: Option[NDArray[Int]]): Try[TriangleMesh[_3D]] = {
      val cellMatrix = ndIntArrayToIntMatrix(cellArray)
      if(cellMatrix.cols != 3) Failure(new Exception("the representer cells are not triangles"))
      else {
        val cells = for (i <- 0 until cellMatrix.rows)
          yield {
            TriangleCell(PointId(cellMatrix(i, 0)), PointId(cellMatrix(i, 1)), PointId(cellMatrix(i, 2)))
          }
        Success(TriangleMesh3D(UnstructuredPoints(points), TriangleList(cells)))
      }
    }

    override def cellsToArray(mesh: TriangleMesh[_3D]): NDArray[Int] = {
      val triangles = mesh.triangulation.triangles
      val cellArray = triangles.map(_.ptId1.id) ++ triangles.map(_.ptId2.id) ++ triangles.map(_.ptId3.id)
      NDArray(IndexedSeq(3, triangles.size), cellArray.toArray)
    }
  }

  implicit object domainIOTetrahedralMesh3D extends StatismoDomainIO[_3D, TetrahedralMesh] {
    override val datasetType: String = "VOLUME_MESH"

    override def createDomainWithCells(points: IndexedSeq[Point[_3D]], cellArray: Option[NDArray[Int]]): Try[TetrahedralMesh[_3D]] = {
      val cellMatrix = ndIntArrayToIntMatrix(cellArray)
      if(cellMatrix.cols != 4) Failure(new Exception("the representer cells are not tetrahedrons"))
      else {
        val cells = for (i <- 0 until cellMatrix.rows)
          yield {
            TetrahedralCell(PointId(cellMatrix(i, 0)), PointId(cellMatrix(i, 1)), PointId(cellMatrix(i, 2)), PointId(cellMatrix(i, 3)))
          }
        Success(TetrahedralMesh3D(UnstructuredPoints(points), TetrahedralList(cells)))
      }
    }

    override def cellsToArray(mesh: TetrahedralMesh[_3D]): NDArray[Int] = {
      val tetrahedrons = mesh.tetrahedralization.tetrahedrons
      val cellArray = tetrahedrons.map(_.ptId1.id) ++ tetrahedrons.map(_.ptId2.id) ++ tetrahedrons.map(_.ptId3.id) ++ tetrahedrons.map(_.ptId4.id)
      NDArray(IndexedSeq(4, tetrahedrons.size), cellArray.toArray)
    }

  }

  implicit object domainIOLineMesh2D extends StatismoDomainIO[_2D, LineMesh] {
    override val datasetType: String = "LINE_MESH"

    override def createDomainWithCells(points: IndexedSeq[Point[_2D]], cellArray: Option[NDArray[Int]]): Try[LineMesh[_2D]] = {
      val cellMatrix = ndIntArrayToIntMatrix(cellArray)
      if(cellMatrix.cols != 2) Failure(new Exception("the representer cells are not lines"))
      else {
        val cells = for (i <- 0 until cellMatrix.rows)
          yield {
            LineCell(PointId(cellMatrix(i, 0)), PointId(cellMatrix(i, 1)))
          }
        Try(LineMesh2D(UnstructuredPoints(points), LineList(cells)))
      }
    }

    override def cellsToArray(mesh: LineMesh[_2D]): NDArray[Int] = {
      val cellArray = mesh.cells.map(_.ptId1.id) ++ mesh.cells.map(_.ptId2.id)
      NDArray(IndexedSeq(2, mesh.cells.size), cellArray.toArray)
    }

  }

  implicit object domainIOLineMesh3D extends StatismoDomainIO[_3D, LineMesh] {
    override val datasetType: String = "LINE_MESH"

    override def createDomainWithCells(points: IndexedSeq[Point[_3D]], cellArray: Option[NDArray[Int]]): Try[LineMesh[_3D]] = {
      val cellMatrix = ndIntArrayToIntMatrix(cellArray)
      if(cellMatrix.cols != 2) Failure(new Exception("the representer cells are not lines"))
      else {
        val cells = for (i <- 0 until cellMatrix.rows)
          yield {
            LineCell(PointId(cellMatrix(i, 0)), PointId(cellMatrix(i, 1)))
          }
        Success(LineMesh3D(UnstructuredPoints(points), LineList(cells)))
      }
    }

    override def cellsToArray(mesh: LineMesh[_3D]): NDArray[Int] = {
      val cellArray = mesh.cells.map(_.ptId1.id) ++ mesh.cells.map(_.ptId2.id)
      NDArray(IndexedSeq(2, mesh.cells.size), cellArray.toArray)
    }

  }


  implicit object domainIOUnstructuredPoints1D extends StatismoDomainIO[_1D, UnstructuredPointsDomain] {
    override val datasetType: String = "POINT_SET"

    override def createDomainWithCells(points: IndexedSeq[Point[_1D]], cellArray: Option[NDArray[Int]]): Try[UnstructuredPointsDomain[_1D]] = {
      Success(CreateUnstructuredPointsDomain1D.create(points))
    }

    override def cellsToArray(mesh: UnstructuredPointsDomain[_1D]): NDArray[Int] = {
      NDArray(IndexedSeq(0), Array.empty)
    }
  }

  implicit object domainIOUnstructuredPoints2D extends StatismoDomainIO[_2D, UnstructuredPointsDomain] {
    override val datasetType: String = "POINT_SET"

    override def createDomainWithCells(points: IndexedSeq[Point[_2D]], cellArray: Option[NDArray[Int]]): Try[UnstructuredPointsDomain[_2D]] = {
      Success(CreateUnstructuredPointsDomain2D.create(points))
    }

    override def cellsToArray(mesh: UnstructuredPointsDomain[_2D]): NDArray[Int] = {
      NDArray(IndexedSeq(0), Array.empty)
    }
  }

  implicit object domainIOUnstructuredPoints3D extends StatismoDomainIO[_3D, UnstructuredPointsDomain] {
    override val datasetType: String = "POINT_SET"

    override def createDomainWithCells(points: IndexedSeq[Point[_3D]], cellArray: Option[NDArray[Int]]): Try[UnstructuredPointsDomain[_3D]] = {
      Success(CreateUnstructuredPointsDomain3D.create(points))
    }

    override def cellsToArray(mesh: UnstructuredPointsDomain[_3D]): NDArray[Int] = {
      NDArray(IndexedSeq(0), Array.empty)
    }
  }


}