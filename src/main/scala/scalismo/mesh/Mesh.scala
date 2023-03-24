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

import scalismo.common.{DifferentiableField, PointId, RealSpace}
import scalismo.geometry._
import scalismo.mesh.boundingSpheres.TriangleMesh3DSpatialIndex

import scala.collection.parallel.immutable.ParVector

/**
 * Defines utility functions on [[TriangleMesh]] instances
 */
object Mesh {

  /**
   * Returns a new continuous [[DifferentiableScalarImage]] defined on 3-dimensional [[RealSpace]] which is the distance
   * transform of the mesh
   */
  @deprecated("Is moved to TriangleMesh3DMeshOperations.", "0.14")
  def meshToDistanceImage(mesh: TriangleMesh[_3D]): DifferentiableField[_3D, Float] = {
    val spIndex = TriangleMesh3DSpatialIndex.fromTriangleMesh3D(mesh)

    def dist(pt: Point[_3D]): Float = Math.sqrt(spIndex.getSquaredShortestDistance(pt)).toFloat

    def grad(pt: Point[_3D]) = {
      val closestPt = spIndex.getClosestPoint(pt).point
      val grad = EuclideanVector(pt(0) - closestPt(0), pt(1) - closestPt(1), pt(2) - closestPt(2))
      grad * (1.0 / grad.norm)
    }
    DifferentiableField(RealSpace[_3D], (pt: Point[_3D]) => dist(pt), (pt: Point[_3D]) => grad(pt))
  }

  /**
   * Returns a new [[TriangleMesh]] where all points satisfying the given predicate are removed. All cells containing
   * deleted points are also deleted.
   */
  @deprecated("Is moved to TriangleMesh3DMeshOperations.", "0.14")
  def clipMesh(mesh: TriangleMesh[_3D], clipPointPredicate: Point[_3D] => Boolean): TriangleMesh[_3D] = {

    // predicate tested at the beginning, once.
    val remainingPoints =
      new ParVector(mesh.pointSet.points.toVector).filter { !clipPointPredicate(_) }.zipWithIndex.toMap
    val pts = mesh.pointSet.points.toIndexedSeq

    val remainingPointTriplet = new ParVector(mesh.cells.toVector)
      .map { cell =>
        val points = cell.pointIds.map(pointId => pts(pointId.id))
        (points, points.map(p => remainingPoints.get(p).isDefined).reduce(_ && _))
      }
      .filter(_._2)
      .map(_._1)

    val points = remainingPointTriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointTriplet.map { case vec =>
      TriangleCell(PointId(pt2Id(vec(0))), PointId(pt2Id(vec(1))), PointId(pt2Id(vec(2))))
    }

    TriangleMesh3D(points.toIndexedSeq, TriangleList(cells.toIndexedSeq))
  }

}
