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
import scalismo.geometry.Dim

import scala.collection.mutable
import scala.collection.Map

/**
 * MeshBoundary is a property to test if points or an edge between two points is on the boundary of the mesh.
 */
trait MeshBoundary {

  /**
   * Check if the point with given index is on the boundary of the underlaying mesh.
   *
   * @return True if point is part of the boundary.
   */
  def pointIsOnBoundary(id: Int): Boolean

  /**
   * Check if the edge between the two points with given indices is on the boundary of the underlaying mesh.
   *
   * @note This method does not check weather there exists a nedge between the two points. You will get false for all
   *       edges not in the mesh.
   * @return True if the edge is part of the boundary.
   */
  def edgeIsOnBoundary(id1: Int, id2: Int): Boolean

}

/**
 * The TriangularMeshBoundary can be queried if a triangle has one side in common with the mesh boundary.
 */
trait TriangularMeshBoundary extends MeshBoundary {

  /**
   * Check if the triangle with given index is on the boundary of the underlaying mesh.
   *
   * @return True if the triangle has at least one border in common with the boundary.
   */
  def triangleIsOnBoundary(id: Int): Boolean
}

/**
 * Implementation of a TriangularMeshBoundary
 *
 * @note the implementation with a Map instead of a CSCMatrix was three times slower using our test mesh.
 */
private class BoundaryOfATriangleMesh(private var vertexIsOnBorder: IndexedSeq[Boolean],
    private var edgeIsOnBorder: CSCMatrix[Boolean],
    private var triangleIsOnBorder: IndexedSeq[Boolean]) extends TriangularMeshBoundary {

  override def pointIsOnBoundary(id: Int): Boolean = {
    return vertexIsOnBorder(id)
  }

  override def edgeIsOnBoundary(id1: Int, id2: Int): Boolean = {
    return edgeIsOnBorder(id1, id2);
  }

  override def triangleIsOnBoundary(id: Int): Boolean = {
    return triangleIsOnBorder(id)
  }
}

/**
 * Factory for mesh boundaries.
 */
object MeshBoundaryFactory {

  /**
   * Build boundary index for triangle mesh.
   *
   * @param mesh Incoming triangle mesh.
   * @return Boundary that can be queried for by index for points, edges, and triangles.
   */
  def triangularMeshBoundary[D <: Dim](mesh: TriangleMesh[D]): TriangularMeshBoundary = {

    val points = mesh.pointSet.points.toIndexedSeq
    val nPts = points.length

    val triangles = mesh.triangulation.triangles
    val nTriangles = triangles.length
    val edgesOfATriangle = Seq((0, 1), (1, 2), (2, 0))

    val pointOnBorder = new Array[Boolean](nPts)
    val triangleOnBorder = new Array[Boolean](nTriangles)

    val edgeOnBorderBuilder = new CSCMatrix.Builder[Boolean](nPts, nPts)
    triangles.map { triangle =>
      edgesOfATriangle.map { edge =>
        val v1 = triangle.pointIds(edge._1).id
        val v2 = triangle.pointIds(edge._2).id
        edgeOnBorderBuilder.add(v1, v2, false)
        edgeOnBorderBuilder.add(v2, v1, false)
      }
    }
    val edgeOnBorderTmp = edgeOnBorderBuilder.result()

    // edges from only a single triangle lie on the border,
    // edges contained in two triangles are not on a border
    triangles.map { triangle =>
      edgesOfATriangle.map { edge =>
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
    triangles.zipWithIndex.map { t =>
      val triangle = t._1
      val tIdx = t._2
      edgesOfATriangle.map { edge =>
        val v1 = triangle.pointIds(edge._1).id
        val v2 = triangle.pointIds(edge._2).id
        if (edgeOnBorder(v1, v2)) {
          pointOnBorder(v1) = true
          pointOnBorder(v2) = true
          triangleOnBorder(tIdx) = true
        }
      }
    }

    new BoundaryOfATriangleMesh(
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