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

import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.mesh.{BarycentricCoordinates, TriangleId, TriangleMesh3D}
import scalismo.tetramesh.{TetrahedralMesh3D, TetrahedronId}

/**
 * The SurfaceIntersectionIndex supports queries about the intersection of a line
 * with a surface. The surface is used to build up he index. For
 * lines in (point,direction) format one can ask if there exists any and also for
 * the complete list of intersection points.
 */
trait SurfaceIntersectionIndex[D] {

  def hasIntersection(point: Point[D], direction: EuclideanVector[D]): Boolean

  def getIntersectionPoints(point: Point[D], direction: EuclideanVector[D]): Seq[Point[D]]

}

trait VolumeIntersectionIndex[D] {

  def hasIntersection(point: Point[D], direction: EuclideanVector[D]): Boolean

  def getIntersectionPoints(point: Point[D], direction: EuclideanVector[D]): Seq[Point[D]]

}


/**
 * The TriangulatedSurfaceIntersectionIndex is a specialization of the SurfaceIntersectionIndex
 * for TriangleMeshs. The additional query return the intersection points in the
 * (TriangleId,BarycentricCoordinates) format.
 */
trait TriangulatedSurfaceIntersectionIndex[D] extends SurfaceIntersectionIndex[D] {

  def getSurfaceIntersectionPoints(point: Point[D], direction: EuclideanVector[D]): Seq[(TriangleId, BarycentricCoordinates)]
}


/**
  * The TetrahedralizedVolumeIntersectionIndex is a specialization of the SurfaceIntersectionIndex
  * for TetrahedralMeshes. The additional query return the intersection points in the
  * (TetrahedronId,BarycentricCoordinates) format.
  */
trait TetrahedralizedVolumeIntersectionIndex[D] extends VolumeIntersectionIndex[D] {

  def getVolumeIntersectionPoints(point: Point[D], direction: EuclideanVector[D]): Seq[(TetrahedronId, BarycentricCoordinates)]
}


/**
  * LineTetrahedralMesh3DIntersecitionIndex implements the interface TriangulatedSurfaceIntersectionIndex for TriangleMesh3D.
  */

object LineTetrahedralMesh3DIntersectionIndex {

  /**
   * Creates VolumeDistance for a TetrahedralMesh3D.
   */
  def fromTetrahedralMesh3D(mesh: TetrahedralMesh3D): TetrahedralizedVolumeIntersectionIndex[_3D] = {

    // build tetrahedral list (use only Vector[_3D], no Points)
    val tetrahedrons = mesh.tetrahedralization.tetrahedrons.map { t =>
      val a = mesh.pointSet.point(t.ptId1).toVector
      val b = mesh.pointSet.point(t.ptId2).toVector
      val c = mesh.pointSet.point(t.ptId3).toVector
      val d = mesh.pointSet.point(t.ptId4).toVector
      new Tetrahedron(a, b, c, d)
    }

    // build up search structure
    val bs = BoundingSpheres.createForTetrahedrons(tetrahedrons)

    // new distance object
    new LineTetrahedralMesh3DIntersectionIndex(bs, mesh, tetrahedrons)

  }

}


/**
  * LineTriangleMesh3DIntersecitionIndex implements the interface TetrahedralizedVolumeIntersectionIndex for TetrahedralMesh3D.
  */
object LineTrinagleMesh3DIntersectionIndex {

  /**
    * Creates SurfaceDistance for a TriangleMesh3D.
    */
  def fromTriangleMesh3D(mesh: TriangleMesh3D): TriangulatedSurfaceIntersectionIndex[_3D] = {

    // build triangle list (use only Vector[_3D], no Points)
    val triangles = mesh.triangulation.triangles.map { t =>
      val a = mesh.pointSet.point(t.ptId1).toVector
      val b = mesh.pointSet.point(t.ptId2).toVector
      val c = mesh.pointSet.point(t.ptId3).toVector
      new Triangle(a, b, c)
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




/**
  * LineTetrahedralMesh3DIntersecitionIndex implements the interface TetrahedralizedvolumeIntersectionIndex for Tetrahedral3D.
  */
 class LineTetrahedralMesh3DIntersectionIndex(private val boundingSphere: BoundingSphere,
                                                        private val mesh: TetrahedralMesh3D,
                                                        private val tetrahedrons: Seq[Tetrahedron]) extends TetrahedralizedVolumeIntersectionIndex[_3D] {

  override def hasIntersection(point: Point[_3D], direction: EuclideanVector[_3D]): Boolean = {
    intersectWithLine(point.toVector, direction, boundingSphere).nonEmpty
  }

  override def getIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[Point[_3D]] = {
    intersectWithLine(point.toVector, direction, boundingSphere)
  }

  override def getVolumeIntersectionPoints(point: Point[_3D], direction: EuclideanVector[_3D]): Seq[(TetrahedronId, BarycentricCoordinates)] = {
    volumeIntersectionPoint(point.toVector, direction, boundingSphere).map(t => (TetrahedronId(t._1), t._2))
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
        val triangle = tetrahedrons(partition.idx)
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

  private def volumeIntersectionPoint(point: EuclideanVector[_3D], direction: EuclideanVector[_3D], partition: BoundingSphere): List[(Int, BarycentricCoordinates)] = {
    if (BSIntersection.intersectLineSphereSquared(point, direction, partition.center, partition.r2)) {
      if (partition.idx < 0) {
        val l = if (partition.hasLeft) {
          volumeIntersectionPoint(point, direction, partition.left)
        } else {
          Nil
        }
        val r = if (partition.hasRight) {
          volumeIntersectionPoint(point, direction, partition.right)
        } else {
          Nil
        }
        l ++ r
      } else {
        val tetrahedron = tetrahedrons(partition.idx)

        var l=List[(Int, BarycentricCoordinates)]()

        for (i <- 0 to 3) {
          val intersection = BSIntersection.intersectLineWithTriangleBarycentric(point, direction, tetrahedron.triangles(i).a, tetrahedron.triangles(i).b, tetrahedron.triangles(i).c)


          if (intersection._1) {
            l=l++List[(Int, BarycentricCoordinates)]((partition.idx, intersection._2))
          } else {
            l=l++List[(Int, BarycentricCoordinates)]()
          }
        }
        l
      }
    } else {
      List[(Int, BarycentricCoordinates)]()
    }
  }

}