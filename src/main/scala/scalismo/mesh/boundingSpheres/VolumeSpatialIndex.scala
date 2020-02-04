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
import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.mesh.boundingSpheres.BSDistance._
import scalismo.mesh.boundingSpheres.SurfaceSpatialIndex.SurfaceClosestPointType
import scalismo.mesh.boundingSpheres.SurfaceSpatialIndex.SurfaceClosestPointType.SurfaceClosestPointType
import scalismo.mesh.boundingSpheres.VolumeSpatialIndex.VolumeClosestPointType.VolumeClosestPointType
import scalismo.mesh.{
  BarycentricCoordinates,
  BarycentricCoordinates4,
  TetrahedralMesh3D,
  TetrahedronId,
  TriangleId,
  TriangleMesh3D
}

/**
 * SurfaceDistance trait with the basic queries defined.
 */
trait VolumeSpatialIndex[D] extends SpatialIndex[D] {

  /**
   * Query the closest point on a surface.
   *
   * @return A desciption of the closest point.
   */
  def getClosestPointToVolume(pt: Point[D]): ClosestPointWithSquaredDistance
}

object VolumeSpatialIndex {

  /**
   * Type of the closest point. At the moment the names are only suited for a triangular mesh.
   */
  private[boundingSpheres] object VolumeClosestPointType extends Enumeration {
    type VolumeClosestPointType = Value
    val POINT, ON_LINE, IN_TRIANGLE, IN_TETRAHEDRON = Value

    def fromSurfaceClosestPointType(scpt: SurfaceClosestPointType) = scpt match {
      case SurfaceClosestPointType.POINT       => POINT
      case SurfaceClosestPointType.ON_LINE     => ON_LINE
      case SurfaceClosestPointType.IN_TRIANGLE => IN_TRIANGLE
    }
  }

}

import VolumeSpatialIndex.VolumeClosestPointType._

private[boundingSpheres] case class VolumeClosestPointMeta(distance2: Double,
                                                           pt: EuclideanVector[_3D],
                                                           ptType: VolumeClosestPointType,
                                                           bc: (Double, Double, Double),
                                                           idx: (Int, Int, Int))

/**
 * Companion object for the surface distance implementation for TriangleMesh3D.
 */
object TetrahedralMesh3DSpatialIndex {

  /**
   * Creates SurfaceDistance for a TriangleMesh3D.
   */
  def fromTetrahedralMesh3D(mesh: TetrahedralMesh3D): VolumeSpatialIndex[_3D] = {

    // build triangle list (use only Vector[_3D], no Points)
    val tetrahedrons = mesh.tetrahedrons.map { t =>
      val a = mesh.pointSet.point(t.ptId1).toVector
      val b = mesh.pointSet.point(t.ptId2).toVector
      val c = mesh.pointSet.point(t.ptId3).toVector
      val d = mesh.pointSet.point(t.ptId4).toVector
      new Tetrahedron(a, b, c, d)
    }

    // build up search structure
    val bs = BoundingSpheres.createForTetrahedrons(tetrahedrons)

    // new distance object
    new TetrahedralMesh3DSpatialIndex(bs, mesh, tetrahedrons)
  }

}

/**
 * Surface distance implementation for TriangleMesh3D.
 */
private[mesh] class TetrahedralMesh3DSpatialIndex(private val bs: BoundingSphere,
                                                  private val mesh: TetrahedralMesh3D,
                                                  private val tetrahedrons: Seq[Tetrahedron])
    extends VolumeSpatialIndex[_3D] {

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
  override def getClosestPointToVolume(point: Point[_3D]): ClosestPointWithSquaredDistance = {

    _getClosestPoint(point)

    // handle found point type
    val tetrahedron = mesh.tetrahedralization.tetrahedron(TetrahedronId(lastIdx.get().idx))
    res.get().ptType match {

      case POINT =>
        ClosestPointIsVertex(res.get().pt.toPoint,
                             res.get().distance2,
                             PointId(tetrahedron.pointIds(res.get().idx._1).id))

      case ON_LINE =>
        val idx = res.get().idx
        val tri = tetrahedron.triangles(idx._1)
        idx match {
          case (_, 0, _) =>
            ClosestPointOnLine(res.get().pt.toPoint,
                               res.get().distance2,
                               (PointId(tri.pointIds(0).id), PointId(tri.pointIds(1).id)),
                               res.get().bc.a)
          case (_, 1, _) =>
            ClosestPointOnLine(res.get().pt.toPoint,
                               res.get().distance2,
                               (PointId(tri.pointIds(1).id), PointId(tri.pointIds(2).id)),
                               res.get().bc.a)
          case (_, 2, _) =>
            ClosestPointOnLine(res.get().pt.toPoint,
                               res.get().distance2,
                               (PointId(tri.pointIds(2).id), PointId(tri.pointIds(0).id)),
                               res.get().bc.a)
          case _ =>
            throw new RuntimeException("not a valid line index")
        }

      case IN_TRIANGLE =>
        ClosestPointInTriangleOfTetrahedron(
          res.get().pt.toPoint,
          res.get().distance2,
          TetrahedronId(lastIdx.get().idx),
          TriangleId(res.get().idx._1),
          BarycentricCoordinates(1.0 - res.get().bc.a - res.get().bc.b, res.get().bc.a, res.get().bc.b)
        )

      case IN_TETRAHEDRON =>
        ClosestPointInTetrahedron(
          res.get().pt.toPoint,
          res.get().distance2,
          TetrahedronId(lastIdx.get().idx),
          BarycentricCoordinates4(1.0 - res.get().bc.a - res.get().bc.b - res.get().bc.c,
                                  res.get().bc.a,
                                  res.get().bc.b,
                                  res.get().bc.c)
        )

      case _ => throw new RuntimeException("not a valid PointType")
    }

  }

  /**
   * Finds the closest point on the surface.
   */
  private def _getClosestPoint(point: Point[_3D]): Unit = {
    val p = point.toVector

    // last triangle might be a good candidate
    val result = BSDistance.toTetrahedron(point.toVector, tetrahedrons(lastIdx.get().idx))
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
  private val res: ThreadLocal[CP3] = new ThreadLocal[CP3]() {
    override protected def initialValue(): CP3 = {
      new CP3(Double.MaxValue, EuclideanVector(-1, -1, -1), POINT, BC3(0, 0, 0), (-1, -1, -1))
    }
  }

  /**
   * Search for the closest point recursively
   */
  private def distanceToPartition(point: EuclideanVector[_3D],
                                  partition: BoundingSphere,
                                  result: CP3,
                                  index: Index): Unit = {
    if (partition.idx >= 0) {
      // we have found a leave
      val res = BSDistance.toTetrahedron(point, tetrahedrons(partition.idx))
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
              (4.0 * distanceToRightCenter * rightRadius >= pow(distanceToRightCenter + rightRadius - result.distance2,
                                                                2))) {
            distanceToPartition(point, partition.right, result, index)
          }
        } else {
          if ((distanceToRightCenter <= rightRadius) ||
              (result.distance2 >= distanceToRightCenter + rightRadius) ||
              (4.0 * distanceToRightCenter * rightRadius >= pow(distanceToRightCenter + rightRadius - result.distance2,
                                                                2))) {
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
  private def updateCP(result: CP3, res: VolumeClosestPointMeta): Unit = {
    result.distance2 = res.distance2
    result.bc = BC3(res.bc._1, res.bc._2, res.bc._3)
    result.idx = res.idx
    result.pt = res.pt
    result.ptType = res.ptType
  }
}
