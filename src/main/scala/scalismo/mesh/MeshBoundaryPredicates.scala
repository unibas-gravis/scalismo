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

import breeze.linalg.CSCMatrix
import scalismo.common.PointId
import scalismo.tetramesh.{TetrahedralMesh, TetrahedronId}

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




trait MeshVolumeBoundaryPredicates {

  /**
    * Check if the point with given index is on the boundary of the underlying meshVolume.
    *
    * @return True if point is part of the boundary.
    */
  def pointIsOnBoundary(id: PointId): Boolean

  /**
    * Check if the edge between the two points with given indices is on the boundary of the underlying mesh Volume.
    *
    * @note This method does not check weather there exists an edge between the two points. You will get false for all
    *       edges not in the mesh Volume.
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
  * The TetrahedraMeshBoundary can be queried if a triangle has one side in common with the mesh boundary.
  */
trait TetrahedralMeshBoundaryPredicates extends MeshVolumeBoundaryPredicates {

  /**
    * Check if the tetrahedron with given index is on the boundary of the underlying mesh volume.
    *
    * @return True if the tetrahedron has at least one border in common with the boundary.
    */
  def tetrahedronIsOnBoundary(id: TetrahedronId): Boolean
}




/**
 * Implementation of a TriangularMeshBoundary
 *
 * @note the implementation with a Map instead of a CSCMatrix was three times slower using our test mesh.
 */

private class BoundaryOfATriangleMeshPredicates(
    private var vertexIsOnBorder: IndexedSeq[Boolean],
    private var edgeIsOnBorder: CSCMatrix[Boolean],
    private var triangleIsOnBorder: IndexedSeq[Boolean]) extends TriangularMeshBoundaryPredicates {

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
  *
  * @note the implementation with a Map instead of a CSCMatrix was three times slower using our test mesh volume.
  */

private class BoundaryOfATetrahedralMeshPredicates(
                                                 private var vertexIsOnBorder: IndexedSeq[Boolean],
                                                 private var edgeIsOnBorder: CSCMatrix[Boolean],
                                                 private var tetrahedronIsOnBorder: IndexedSeq[Boolean]) extends TetrahedralMeshBoundaryPredicates {

  override def pointIsOnBoundary(id: PointId): Boolean = {
    vertexIsOnBorder(id.id)
  }

  override def edgeIsOnBoundary(id1: PointId, id2: PointId): Boolean = {
    edgeIsOnBorder(id1.id, id2.id)
  }

  override def tetrahedronIsOnBoundary(id: TetrahedronId): Boolean = {
    tetrahedronIsOnBorder(id.id)
  }
}




/**
 * Factory for mesh boundaries.
 */
object MeshBoundaryPredicates {

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

    new BoundaryOfATriangleMeshPredicates(
      pointOnBorder.toIndexedSeq,
      edgeOnBorder,
      triangleOnBorder.toIndexedSeq
    )
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




/**
  * Factory for mesh volume boundaries.
  */
object MeshVolumeBoundaryPredicates {

  /**
    * Build boundary index for tetrahedral mesh.
    *
    * @param mesh volume Incoming tetrahedral mesh.
    * @return Boundary that can be queried for by index for points, edges, and etrahedrons.
    */
  def apply[D](mesh: TetrahedralMesh[D]): TetrahedralMeshBoundaryPredicates = {

    val points = mesh.pointSet.points.toIndexedSeq
    val nPts = points.length

    val tetrahedrons = mesh.tetrahedralization.tetrahedrons
    val nTetrahedrons = tetrahedrons.length
    val edgesOfATetrahedron = Seq((0, 1), (0, 2), (0, 3),(1,2), (1,3), (2,3))

    val pointOnBorder = new Array[Boolean](nPts)
    val tetrahedronOnBorder = new Array[Boolean](nTetrahedrons)

    val edgeOnBorderBuilder = new CSCMatrix.Builder[Boolean](nPts, nPts)
    tetrahedrons.foreach { tetrahedron =>
      edgesOfATetrahedron.foreach { edge =>
        val v1 = tetrahedron.pointIds(edge._1).id
        val v2 = tetrahedron.pointIds(edge._2).id
        edgeOnBorderBuilder.add(v1, v2, false)
        edgeOnBorderBuilder.add(v2, v1, false)
      }
    }
    val edgeOnBorderTmp = edgeOnBorderBuilder.result()

    // edges from only a single tetrahedron lie on the border,
    // edges contained in two tetrahedrons are not on a border
    tetrahedrons.foreach { tetrahedron =>
      edgesOfATetrahedron.foreach { edge =>
        val v1 = tetrahedron.pointIds(edge._1).id
        val v2 = tetrahedron.pointIds(edge._2).id
        edgeOnBorderTmp(v1, v2) = !edgeOnBorderTmp(v1, v2)
        edgeOnBorderTmp(v2, v1) = !edgeOnBorderTmp(v2, v1)
      }
    }

    // optimization : we do not need values corresponding to false in the sparse representation
    val edgeOnBorder = reduceCSCMatrixBooleansToTrueEntries(edgeOnBorderTmp)

    // points at one end of a border edge are also on the border,
    // triangles with one side on the border are also on the border
    tetrahedrons.zipWithIndex.foreach { t =>
      val tetrahedron = t._1
      val tIdx = t._2
      edgesOfATetrahedron.foreach { edge =>
        val v1 = tetrahedron.pointIds(edge._1).id
        val v2 = tetrahedron.pointIds(edge._2).id
        if (edgeOnBorder(v1, v2)) {
          pointOnBorder(v1) = true
          pointOnBorder(v2) = true
          tetrahedronOnBorder(tIdx) = true
        }
      }
    }

    new BoundaryOfATetrahedralMeshPredicates(
      pointOnBorder.toIndexedSeq,
      edgeOnBorder,
      tetrahedronOnBorder.toIndexedSeq
    )
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