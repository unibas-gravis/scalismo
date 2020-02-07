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

import breeze.linalg
import breeze.linalg.CSCMatrix
import scalismo.common.PointId

/**
 * MeshBoundary is a property to test if points or an edge between two points is on the boundary of the mesh.
 */
trait MeshBoundaryPredicates {

  /**
   * Check if the point with given index is on the boundary of the underlying mesh.
   *
   * @return True if point is part of the boundary.
   */
  def pointIsOnBoundary(id: PointId): Boolean

  /**
   * Check if the edge between the two points with given indices is on the boundary of the underlying mesh.
   *
   * @note This method does not check weather there exists an edge between the two points. You will get false for all
   *       edges not in the mesh.
   * @return True if the edge is part of the boundary.
   */
  def edgeIsOnBoundary(id1: PointId, id2: PointId): Boolean

}

/**
 * The TriangularMeshBoundary can be queried if a triangle has one side in common with the mesh boundary.
 */
trait TriangularMeshBoundaryPredicates extends MeshBoundaryPredicates {

  /**
   * Check if the triangle with given index is on the boundary of the underlying mesh.
   *
   * @return True if the triangle has at least one border in common with the boundary.
   */
  def triangleIsOnBoundary(id: TriangleId): Boolean
}

/**
 * The TetrahedralMeshBoundary can be queried if a tetrahedron has one side in common with the mesh surface.
 */
trait TetrahedralMeshBoundaryPredicates extends MeshBoundaryPredicates {

  /**
   * Check if the tetrahedron with given index is on the boundary of the underlying mesh.
   *
   * @return True if the tetrahedron has at least one face in common with the boundary.
   */
  def tetrahedronIsOnBoundary(id: TetrahedronId): Boolean

  def triangleIsOnBoundary(tc: TriangleCell): Boolean
}

/**
 * Implementation of a TriangularMeshBoundary
 *
 * @note the implementation with a Map instead of a CSCMatrix was three times slower using our test mesh.
 */
private class BoundaryOfATriangleMeshPredicates(private val vertexIsOnBorder: IndexedSeq[Boolean],
                                                private val edgeIsOnBorder: CSCMatrix[Boolean],
                                                private val triangleIsOnBorder: IndexedSeq[Boolean])
    extends TriangularMeshBoundaryPredicates {

  override def pointIsOnBoundary(id: PointId): Boolean = {
    vertexIsOnBorder(id.id)
  }

  override def edgeIsOnBoundary(id1: PointId, id2: PointId): Boolean = {
    edgeIsOnBorder(id1.id, id2.id)
  }

  override def triangleIsOnBoundary(id: TriangleId): Boolean = {
    triangleIsOnBorder(id.id)
  }
}

/**
 * Implementation of a TetrahedralMeshBoundary
 */
private class BoundaryOfATetrahedralMeshPredicates(private val vertexIsOnBorder: IndexedSeq[Boolean],
                                                   private val edgeIsOnBorder: CSCMatrix[Boolean],
                                                   private val triangleIsOnBorder: Map[TriangleCell, Boolean],
                                                   private val tetrahedronIsOnBorder: IndexedSeq[Boolean])
    extends TetrahedralMeshBoundaryPredicates {

  override def pointIsOnBoundary(id: PointId): Boolean = {
    vertexIsOnBorder(id.id)
  }

  override def edgeIsOnBoundary(id1: PointId, id2: PointId): Boolean = {
    edgeIsOnBorder(id1.id, id2.id)
  }

  override def triangleIsOnBoundary(tc: TriangleCell): Boolean = {
    triangleIsOnBorder.contains(tc)
  }

  override def tetrahedronIsOnBoundary(id: TetrahedronId): Boolean = {
    tetrahedronIsOnBorder(id.id)
  }
}

/**
 * Factory for mesh boundaries.
 */
object MeshBoundaryPredicates {

  // get triangles on outside
  private[mesh] class TriangleSortedPointIds(a: PointId, b: PointId, c: PointId)
  private[mesh] object TriangleSortedPointIds {
    def apply(pids: Seq[PointId]) = {
      require(pids.size == 3, "To create a triangle, three values are required.")
      val sorted = pids.map(_.id).sorted
      (sorted(0), sorted(1), sorted(2))
    }
  }

  /**
   * Build boundary index for triangle mesh.
   *
   * @param mesh Incoming triangle mesh.
   * @return Boundary that can be queried for by index for points, edges, and triangles.
   */
  def apply[D](mesh: TriangleMesh[D]): TriangularMeshBoundaryPredicates = {

    val points = mesh.pointSet.points.toIndexedSeq
    val nPts = points.length

    val triangles = mesh.triangulation.triangles
    val nTriangles = triangles.length
    val edgesOfATriangle = Seq((0, 1), (1, 2), (2, 0))

    val pointOnBorder = new Array[Boolean](nPts)
    val triangleOnBorder = new Array[Boolean](nTriangles)

    val edgeOnBorderBuilder = new CSCMatrix.Builder[Boolean](nPts, nPts)
    triangles.foreach { triangle =>
      edgesOfATriangle.foreach { edge =>
        val v1 = triangle.pointIds(edge._1).id
        val v2 = triangle.pointIds(edge._2).id
        edgeOnBorderBuilder.add(v1, v2, false)
        edgeOnBorderBuilder.add(v2, v1, false)
      }
    }
    val edgeOnBorderTmp = edgeOnBorderBuilder.result()

    // edges from only a single triangle lie on the border,
    // edges contained in two triangles are not on a border
    triangles.foreach { triangle =>
      edgesOfATriangle.foreach { edge =>
        val v1 = triangle.pointIds(edge._1).id
        val v2 = triangle.pointIds(edge._2).id
        edgeOnBorderTmp(v1, v2) = !edgeOnBorderTmp(v1, v2)
        edgeOnBorderTmp(v2, v1) = !edgeOnBorderTmp(v2, v1)
      }
    }

    // optimization : we do not need values corresponding to false in the sparse representation
    val edgeOnBorder = reduceCSCMatrixBooleansToTrueEntries(edgeOnBorderTmp)

    // points at one end of a border edge are also on the border,
    // triangles with one side on the border are also on the border
    triangles.zipWithIndex.foreach { t =>
      val triangle = t._1
      val tIdx = t._2
      edgesOfATriangle.foreach { edge =>
        val v1 = triangle.pointIds(edge._1).id
        val v2 = triangle.pointIds(edge._2).id
        if (edgeOnBorder(v1, v2)) {
          pointOnBorder(v1) = true
          pointOnBorder(v2) = true
          triangleOnBorder(tIdx) = true
        }
      }
    }

    new BoundaryOfATriangleMeshPredicates(pointOnBorder.toIndexedSeq, edgeOnBorder, triangleOnBorder.toIndexedSeq)
  }

  /**
   * Build boundary index for triangle mesh.
   *
   * @param mesh Incoming triangle mesh.
   * @return Boundary that can be queried for by index for points, edges, and triangles.
   */
  def apply[D](mesh: TetrahedralMesh[D]): TetrahedralMeshBoundaryPredicates = {

    val tetrahedrons = mesh.tetrahedralization.tetrahedrons
    val triangleOnBorder = fillTriangleOnBorderMap(tetrahedrons)

    // calculate points, lines, and tetrahedrons on outside
    val edgesOfATriangle = Seq((0, 1), (1, 2), (2, 0))
    val tetrahedronsOnBoundary = new Array[Boolean](tetrahedrons.size)
    val pointsOnBoundary = new Array[Boolean](mesh.pointSet.numberOfPoints)
    val edgeOnBoundaryBuilder =
      new linalg.CSCMatrix.Builder[Boolean](mesh.pointSet.numberOfPoints, mesh.pointSet.numberOfPoints)
    val trianglesOnBoundaryMap = scala.collection.mutable.Map[TriangleCell, Boolean]()
    tetrahedrons.zipWithIndex.foreach {
      case (tet, idx) =>
        val trianglesOnBoundary =
          tet.triangles.filter(tri => triangleOnBorder.contains(TriangleSortedPointIds(tri.pointIds)))
        if (trianglesOnBoundary.nonEmpty) {
          tetrahedronsOnBoundary(idx) = true
          trianglesOnBoundary.foreach { tri =>
            trianglesOnBoundaryMap(tri) = true
            tri.pointIds.foreach(pid => pointsOnBoundary(pid.id) = true)
            edgesOfATriangle.map { e =>
              val v1 = tri.pointIds(e._1).id
              val v2 = tri.pointIds(e._2).id
              edgeOnBoundaryBuilder.add(v1, v2, true)
              edgeOnBoundaryBuilder.add(v2, v1, true)
            }
          }
        }
    }
    val edgeOnBorder = reduceCSCMatrixBooleansToTrueEntries(edgeOnBoundaryBuilder.result)

    new BoundaryOfATetrahedralMeshPredicates(pointsOnBoundary.toIndexedSeq,
                                             edgeOnBorder,
                                             trianglesOnBoundaryMap.toMap,
                                             tetrahedronsOnBoundary.toIndexedSeq)
  }

  private[mesh] def fillTriangleOnBorderMap[D](
    tetrahedrons: IndexedSeq[TetrahedralCell]
  ) = {
    val triangleOnBorder = scala.collection.mutable.Map[(Int, Int, Int), Boolean]()

    val uniqueTriangles = tetrahedrons.flatMap { tet =>
      tet.triangles.map { tri =>
        TriangleSortedPointIds(tri.pointIds)
      }
    }.distinct

    // find triangles on border
    uniqueTriangles.foreach(tri => triangleOnBorder(tri) = false)

    tetrahedrons.foreach { tet =>
      tet.triangles.foreach { tri =>
        val triangle = TriangleSortedPointIds(tri.pointIds)
        triangleOnBorder(triangle) = !triangleOnBorder(triangle)
      }
    }
    triangleOnBorder.filter(_._2).toMap
  }

  /**
   * Reduces a boolean CSCMatrix so that only the true entries are contained. Everything else is false anyway.
   */
  private def reduceCSCMatrixBooleansToTrueEntries(m: CSCMatrix[Boolean]): CSCMatrix[Boolean] = {
    val reduced = new CSCMatrix.Builder[Boolean](m.cols, m.rows)

    val it = m.activeIterator
    it.foreach { e =>
      val value = e._2
      val idx = e._1
      if (value)
        reduced.add(idx._1, idx._2, value)
    }
    reduced.result
  }

}
