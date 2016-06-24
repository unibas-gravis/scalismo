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


/**
  * MeshBoundary is a property to test if points or a line between two points is on the boundary of the mesh.
  */
trait MeshBoundary {

  /**
    * Check if the point with given index is on the boundary of the underlaying mesh.
    *
    * @return True if point is part of the boundary.
    */
  def pointIsOnBoundary(id: Int): Boolean

  /**
    * Check if the line between the two points with given indices is on the boundary of the underlaying mesh.
    * @note This method does not check weather there exists a line between the two points. You will get false for all
    *       lines not in the mesh.
    *
    * @return True if the line is part of the boundary.
    */
  def lineIsOnBoundary(id1: Int, id2: Int): Boolean

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
  */
private class BoundaryOfATriangleMesh(private var vertexIsOnBorder: IndexedSeq[Boolean],
                                      private var lineIsOnBorder: CSCMatrix[Boolean], // NOTE: CSCMatrix is a yet experimental sparse matrix in breeze
                                      private var triangleIsOnBorder: IndexedSeq[Boolean]) extends TriangularMeshBoundary {

  override def pointIsOnBoundary(id: Int) : Boolean = {
    return vertexIsOnBorder(id)
  }

  override def lineIsOnBoundary(id1: Int, id2: Int): Boolean = {
    return lineIsOnBorder(id1,id2);
  }

  override def triangleIsOnBoundary(id: Int ) : Boolean = {
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
    * @return Boundary that can be queried for by index for points, lines, and triangles.
    */
  def triangularMeshBoundary[D <: Dim](mesh: TriangleMesh[D]): TriangularMeshBoundary = {

    val points = mesh.pointSet.points.toIndexedSeq
    val nPts = points.length

    val triangles = mesh.triangulation.triangles
    val nTriangles = triangles.length
    val linesOfATriangle = Seq((0,1),(1,2),(2,0))

    val pointOnBorder = new Array[Boolean](nPts)
    val lineOnBorder = new CSCMatrix.Builder[Boolean](nPts,nPts).result()
    val triangleOnBorder = new Array[Boolean](nTriangles)

    // lines in only a single triangle lie on the border,
    // lines contained in two triangles are not on a border
    triangles.map{ triangle =>
      linesOfATriangle.map{ line =>
        val v1 = triangle.pointIds(line._1).id
        val v2 = triangle.pointIds(line._2).id
        lineOnBorder(v1,v2) = ! lineOnBorder(v1,v2)
        lineOnBorder(v2,v1) = ! lineOnBorder(v2,v1)
      }
    }

    // points at one end of a border line are also on the border,
    // triangles with one side on the border are also on the border
    triangles.zipWithIndex.map{ t =>
      val triangle = t._1
      val tIdx = t._2
      linesOfATriangle.map{ line =>
        val v1 = triangle.pointIds(line._1).id
        val v2 = triangle.pointIds(line._2).id
        if( lineOnBorder(v1,v2) ) {
          pointOnBorder(v1) = true
          pointOnBorder(v2) = true
          triangleOnBorder(tIdx) = true
        }
      }
    }

    new BoundaryOfATriangleMesh(
      pointOnBorder.toIndexedSeq,
      lineOnBorder,
      triangleOnBorder.toIndexedSeq
    )
  }

}