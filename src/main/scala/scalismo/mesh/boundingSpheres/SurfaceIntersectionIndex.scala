/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.mesh.boundingSpheres

import scalismo.geometry.{ Dim, Point, EuclideanVector, _3D }
import scalismo.mesh.{ BarycentricCoordinates, TriangleId, TriangleMesh3D }

/**
 * The SurfaceIntersectionIndex supports queries about the intersection of a line
 * with a surface. The surface is used to build up he index. For
 * lines in (point,direction) format one can ask if there exists any and also for
 * the complete list of intersection points.
 */
trait SurfaceIntersectionIndex[D <: Dim] {

  def hasIntersection(point: Point[D], direction: EuclideanVector[D]): Boolean

  def getIntersectionPoints(point: Point[D], direction: EuclideanVector[D]): Seq[Point[D]]

}

/**
 * The TriangulatedSurfaceIntersectionIndex is a specialization of the SurfaceIntersectionIndex
 * for TriangleMeshs. The additional query return the intersection points in the
 * (TriangleId,BarycentricCoordinates) format.
 */
trait TriangulatedSurfaceIntersectionIndex[D <: Dim] extends SurfaceIntersectionIndex[D] {

  def getSurfaceIntersectionPoints(point: Point[D], direction: EuclideanVector[D]): Seq[(TriangleId, BarycentricCoordinates)]
}

/**
 * LineTriangleMesh3DIntersecitionIndex implements the interface TriangulatedSurfaceIntersectionIndex for TriangleMesh3D.
 */
object LineTriangleMesh3DIntersectionIndex {

  /**
   * Creates SurfaceDistance for a TriangleMesh3D.
   */
  def fromTriangleMesh3D(mesh: TriangleMesh3D): TriangulatedSurfaceIntersectionIndex[_3D] = {

    // build triangle list (use only Vector[_3D], no Points)
    val triangles = mesh.triangulation.triangles.map { t =>

      val a = mesh.pointSet.point(t.ptId1).toVector
      val b = mesh.pointSet.point(t.ptId2).toVector
      val c = mesh.pointSet.point(t.ptId3).toVector
      val ab = b - a
      val ac = c - a

      new Triangle(
        a, b, c,
        ab, ac,
        ab.crossproduct(ac)
      )

    }

    // build up search structure
    val bs = BoundingSpheres.createForTriangles(triangles)

    // new distance object
    new LineTriangleMesh3DIntersectionIndex(bs, mesh, triangles)

  }

}

/**
 * LineTriangleMesh3DIntersecitionIndex implements the interface TriangulatedSurfaceIntersectionIndex for TriangleMesh3D.
 */
private[mesh] class LineTriangleMesh3DIntersectionIndex(private val boundingSphere: BoundingSphere,
    private val mesh: TriangleMesh3D,
    private val triangles: Seq[Triangle]) extends TriangulatedSurfaceIntersectionIndex[_3D] {

  override def hasIntersection(point: Point[_3D], direction: EuclideanVector[_3D]): Boolean = {
    intersectWithLine(point.toVector, direction, boundingSphere).nonEmpty
  }

  override def getIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[Point[_3D]] = {
    intersectWithLine(point.toVector, direction, boundingSphere)
  }

  override def getSurfaceIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[(TriangleId, BarycentricCoordinates)] = {
    surfaceIntersectionPoint(point.toVector, direction, boundingSphere).map(t => (TriangleId(t._1), t._2))
  }

  private def intersectWithLine(point: EuclideanVector[_3D], direction: EuclideanVector[_3D], partition: BoundingSphere): Seq[Point[_3D]] = {
    if (BSIntersection.intersectLineSphereSquared(point, direction, partition.center, partition.r2)) {
      if (partition.idx < 0) {
        val l = if (partition.hasLeft) {
          intersectWithLine(point, direction, partition.left)
        } else {
          Nil
        }
        val r = if (partition.hasRight) {
          intersectWithLine(point, direction, partition.right)
        } else {
          Nil
        }
        l ++ r
      } else {
        val triangle = triangles(partition.idx)
        val intersection: (Boolean, Point[_3D]) = BSIntersection.intersectLineWithTriangle(point, direction, triangle.a, triangle.b, triangle.c)
        if (intersection._1) {
          List[Point[_3D]](intersection._2)
        } else {
          List[Point[_3D]]()
        }
      }
    } else {
      List[Point[_3D]]()
    }
  }

  private def surfaceIntersectionPoint(point: EuclideanVector[_3D], direction: EuclideanVector[_3D], partition: BoundingSphere): List[(Int, BarycentricCoordinates)] = {
    if (BSIntersection.intersectLineSphereSquared(point, direction, partition.center, partition.r2)) {
      if (partition.idx < 0) {
        val l = if (partition.hasLeft) {
          surfaceIntersectionPoint(point, direction, partition.left)
        } else {
          Nil
        }
        val r = if (partition.hasRight) {
          surfaceIntersectionPoint(point, direction, partition.right)
        } else {
          Nil
        }
        l ++ r
      } else {
        val triangle = triangles(partition.idx)
        val intersection = BSIntersection.intersectLineWithTriangleBarycentric(point, direction, triangle.a, triangle.b, triangle.c)
        if (intersection._1) {
          List[(Int, BarycentricCoordinates)]((partition.idx, intersection._2))
        } else {
          List[(Int, BarycentricCoordinates)]()
        }
      }
    } else {
      List[(Int, BarycentricCoordinates)]()
    }
  }

}