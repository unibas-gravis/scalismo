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

import scalismo.common.{ UnstructuredPointsDomain, PointId, RealSpace }
import scalismo.geometry._
import scalismo.image.{ DifferentiableScalarImage, ScalarImage }

/**
 * Defines utility functions on [[TriangleMesh]] instances
 */
object Mesh {

  /**
   * Returns a new continuous [[DifferentiableScalarImage]] defined on 3-dimensional [[RealSpace]] which is the distance transform of the mesh
   */
  def meshToDistanceImage(mesh: TriangleMesh[_3D]): DifferentiableScalarImage[_3D] = {

    def dist(pt: Point[_3D]): Float = {
      val closestPt = mesh.pointSet.findClosestPoint(pt).point
      Math.sqrt(Math.pow(closestPt(0) - pt(0), 2) + Math.pow(closestPt(1) - pt(1), 2) + Math.pow(closestPt(2) - pt(2), 2)).toFloat
    }
    def grad(pt: Point[_3D]) = {
      val closestPt = mesh.pointSet.findClosestPoint(pt).point
      val grad = Vector(pt(0) - closestPt(0), pt(1) - closestPt(1), pt(2) - closestPt(2))
      grad * (1.0 / grad.norm)
    }
    DifferentiableScalarImage(RealSpace[_3D], (pt: Point[_3D]) => dist(pt), (pt: Point[_3D]) => grad(pt))
  }

  /**
   * Returns a new continuous binary [[ScalarImage]] defined on 3-dimensional [[RealSpace]] , where the mesh surface is used to split the image domain.
   * Points lying on the space side pointed towards by the surface normals will have value 0. Points lying on the other side have
   * value 1. Hence if the mesh is a closed surface, points inside the surface have value 1 and points outside 0.
   *
   */
  def meshToBinaryImage(mesh: TriangleMesh[_3D]): ScalarImage[_3D] = {
    def inside(pt: Point[_3D]): Short = {
      val closestMeshPt = mesh.pointSet.findClosestPoint(pt)
      val dotprod = mesh.vertexNormals(closestMeshPt.id) dot (closestMeshPt.point - pt)
      if (dotprod > 0.0) 1 else 0
    }

    ScalarImage(RealSpace[_3D], (pt: Point[_3D]) => inside(pt))
  }

  /**
   * Returns a new [[TriangleMesh]] where all points satisfying the given predicate are removed.
   * All cells containing deleted points are also deleted.
   *
   */

  def clipMesh(mesh: TriangleMesh[_3D], clipPointPredicate: Point[_3D] => Boolean): TriangleMesh[_3D] = {

    // predicate tested at the beginning, once.
    val remainingPoints = mesh.pointSet.points.toIndexedSeq.par.filter { !clipPointPredicate(_) }.zipWithIndex.toMap
    val pts = mesh.pointSet.points.toIndexedSeq

    val remainingPointTriplet = mesh.cells.par.map {
      cell =>
        val points = cell.pointIds.map(pointId => pts(pointId.id))
        (points, points.map(p => remainingPoints.get(p).isDefined).reduce(_ && _))
    }.filter(_._2).map(_._1)

    val points = remainingPointTriplet.flatten.distinct
    val pt2Id = points.zipWithIndex.toMap
    val cells = remainingPointTriplet.map { case vec => TriangleCell(PointId(pt2Id(vec(0))), PointId(pt2Id(vec(1))), PointId(pt2Id(vec(2)))) }

    TriangleMesh3D(points.toIndexedSeq, TriangleList(cells.toIndexedSeq))
  }

}