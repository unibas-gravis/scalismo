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
package scalismo.mesh.boundingSpheres

import breeze.numerics.pow
import scalismo.common.PointId
import scalismo.geometry.{ Dim, Point, Vector, _3D }
import scalismo.mesh.{ BarycentricCoordinates, TriangleId, TriangleMesh3D }
import scalismo.mesh.boundingSpheres.BSDistance._

/**
 * SurfaceDistance trait with the basic queries defined.
 */
trait SurfaceSpatialIndex[D <: Dim] {

  /**
   * Query the shortest distance for a point to a surface.
   *
   * @return Squared distance.
   */
  def getSquaredShortestDistance(pt: Point[D]): Double

  /**
   * Query the closest point on a surface.
   *
   * @return The closest point and the squared distance.
   */
  def getClosestPoint(pt: Point[D]): ClosestPoint

  /**
   * Query the closest point on a surface.
   *
   * @return A desciption of the closest point.
   */
  def getClosestPointMeta(pt: Point[D]): ClosestPoint
}

/**
 * Type of the closest point. At the moment the names are only suited for a triangular mesh.
 */
private object ClosestPointType extends Enumeration {
  type ClosestPointType = Value
  val POINT, ON_LINE, IN_TRIANGLE = Value
}

import ClosestPointType._

/**
 * Descritpion of a closest point
 *
 * @param distance2 the squared distance
 * @param pt        the coordinates
 * @param ptType    on which geometric entity of the surface the closest point lies
 * @param bc        the barycentric coordinates of the point. The interpretation depends on the ptType.
 * @param idx       the index in the original surface instance of the geometric entity where the closest point lies. The interpretation depends on the ptType.
 */
private case class ClosestPointMeta(distance2: Double,
  pt: Vector[_3D],
  ptType: ClosestPointType,
  bc: (Double, Double),
  idx: (Int, Int))

/**
 * Companion object for the surface distance implementation for TriangleMesh3D.
 */
object TriangleMesh3DSpatialIndex {

  /**
   * Creates SurfaceDistance for a TriangleMesh3D.
   */
  def fromTriangleMesh3D(mesh: TriangleMesh3D): SurfaceSpatialIndex[_3D] = {

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
    new TriangleMesh3DSpatialIndex(bs, mesh, triangles)
  }

}

/**
 * Surface distance implementation for TriangleMesh3D.
 */
private[mesh] class TriangleMesh3DSpatialIndex(private val bs: BoundingSphere,
  private val mesh: TriangleMesh3D,
  private val triangles: Seq[Triangle])
    extends SurfaceSpatialIndex[_3D] {

  /**
   * Calculates the squared closest distance to the surface.
   */
  override def getSquaredShortestDistance(point: Point[_3D]): Double = {
    _getClosestPoint(point)
    res.get().distance2
  }

  /**
   * Calculates the point and the squared closest distance to the surface.
   */
  override def getClosestPoint(point: Point[_3D]): ClosestPoint = {
    _getClosestPoint(point)
    new ClosestPoint(res.get().pt.toPoint, res.get().distance2)
  }

  /**
   * Returns a description of the closest Point on the surface.
   */
  override def getClosestPointMeta(point: Point[_3D]): ClosestPoint = {

    _getClosestPoint(point)

    // handle found point type
    val triangle = mesh.triangulation.triangle(TriangleId(lastIdx.get().idx))
    res.get().ptType match {

      case POINT => ClosestPointIsVertex(res.get().pt.toPoint, res.get().distance2, PointId(triangle.pointIds(res.get().idx._1).id))

      case ON_LINE => res.get().idx match {
        case (0, _) => ClosestPointOnLine(res.get().pt.toPoint, res.get().distance2, (PointId(triangle.pointIds(0).id), PointId(triangle.pointIds(1).id)), res.get().bc.a)
        case (1, _) => ClosestPointOnLine(res.get().pt.toPoint, res.get().distance2, (PointId(triangle.pointIds(1).id), PointId(triangle.pointIds(2).id)), res.get().bc.a)
        case (2, _) => ClosestPointOnLine(res.get().pt.toPoint, res.get().distance2, (PointId(triangle.pointIds(2).id), PointId(triangle.pointIds(0).id)), res.get().bc.a)
        case _ => throw new RuntimeException("not a valid line index")
      }

      case IN_TRIANGLE => ClosestPointInTriangle(res.get().pt.toPoint, res.get().distance2, TriangleId(lastIdx.get().idx), BarycentricCoordinates(1.0 - res.get().bc.a - res.get().bc.b, res.get().bc.a, res.get().bc.b))

      case _ => throw new RuntimeException("not a valid PointType")
    }

  }

  /**
   * Finds the closest point on the surface.
   */
  private def _getClosestPoint(point: Point[_3D]): Unit = {
    val p = point.toVector

    // last triangle might be a good candidate
    val result = BSDistance.toTriangle(point.toVector, triangles(lastIdx.get().idx))
    updateCP(res.get(), result)

    // search for true candidate
    distanceToPartition(p, bs, res.get(), lastIdx.get())
  }

  /** @note both values contain a mutable state, this is needed to improve speed when having many queries with successive near points. */
  private val lastIdx: ThreadLocal[Index] = new ThreadLocal[Index]() {
    override protected def initialValue(): Index = {
      new Index(0)
    }
  }
  private val res: ThreadLocal[CP] = new ThreadLocal[CP]() {
    override protected def initialValue(): CP = {
      new CP(Double.MaxValue, Vector(-1, -1, -1), POINT, BC(0, 0), (-1, -1))
    }
  }

  /**
   * Search for the closest point recursively
   */
  private def distanceToPartition(point: Vector[_3D],
    partition: BoundingSphere,
    result: CP,
    index: Index): Unit = {
    if (partition.idx >= 0) {
      // we have found a leave
      val res = BSDistance.toTriangle(point, triangles(partition.idx))
      if (res.distance2 < result.distance2) {
        updateCP(result, res)
        index.idx = partition.idx
      }
    } else {
      if (partition.hasLeft && partition.hasRight) {

        val distanceToLeftCenter = (point - partition.left.center).norm2
        val distanceToRightCenter = (point - partition.right.center).norm2
        val leftRadius = partition.left.r2
        val rightRadius = partition.right.r2

        if (distanceToLeftCenter < distanceToRightCenter) {
          // nearer sphere first
          if ((distanceToLeftCenter <= leftRadius) || // point in sphere?
            (result.distance2 >= distanceToLeftCenter + leftRadius) || // are we close?
            (4.0 * distanceToLeftCenter * leftRadius >= pow(distanceToLeftCenter + leftRadius - result.distance2, 2))) {
            // even better estimation
            distanceToPartition(point, partition.left, result, index) // test partition
          }
          if ((distanceToRightCenter <= rightRadius) ||
            (result.distance2 >= distanceToRightCenter + rightRadius) ||
            (4.0 * distanceToRightCenter * rightRadius >= pow(distanceToRightCenter + rightRadius - result.distance2, 2))) {
            distanceToPartition(point, partition.right, result, index)
          }
        } else {
          if ((distanceToRightCenter <= rightRadius) ||
            (result.distance2 >= distanceToRightCenter + rightRadius) ||
            (4.0 * distanceToRightCenter * rightRadius >= pow(distanceToRightCenter + rightRadius - result.distance2, 2))) {
            distanceToPartition(point, partition.right, result, index)
          }
          if ((distanceToLeftCenter <= leftRadius) ||
            (result.distance2 >= distanceToLeftCenter + leftRadius) ||
            (4.0 * distanceToLeftCenter * leftRadius >= pow(distanceToLeftCenter + leftRadius - result.distance2, 2))) {
            distanceToPartition(point, partition.left, result, index)
          }
        }
      } else {
        if (partition.hasLeft) {

          val lc2 = (point - partition.left.center).norm2
          val lr2 = partition.left.r2

          if ((lc2 <= lr2) ||
            (result.distance2 >= lc2 + lr2) ||
            (4.0 * lc2 * lr2 >= pow(lc2 + lr2 - result.distance2, 2))) {
            distanceToPartition(point, partition.left, result, index)
          }
        } else if (partition.hasRight) {
          val rc2 = (point - partition.right.center).norm2
          val rr2 = partition.right.r2
          if ((rc2 <= rr2) ||
            (result.distance2 >= rc2 + rr2) ||
            (4.0 * rc2 * rr2 >= pow(rc2 + rr2 - result.distance2, 2))) {
            distanceToPartition(point, partition.right, result, index)
          }
        }
      }
    }
  }

  /**
   * Helper function to update the mutable case class with immutable sibling.
   */
  private def updateCP(result: CP, res: ClosestPointMeta): Unit = {
    result.distance2 = res.distance2
    result.bc = BC(res.bc._1, res.bc._2)
    result.idx = res.idx
    result.pt = res.pt
    result.ptType = res.ptType
  }
}

