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

import scalismo.common.PointId
import scalismo.geometry.{_3D, Point}
import scalismo.mesh.{BarycentricCoordinates, BarycentricCoordinates4, TetrahedronId, TriangleId}

/**
 * A class that contains the location and the distance to the closest point on a surface.
 * @param point The closest point location on the surface.
 * @param distanceSquared The squared distance to the closest point location.
 */
case class ClosestPoint(point: Point[_3D], distanceSquared: Double) {

  def distance = Math.sqrt(distanceSquared)

  def <(that: ClosestPoint): Boolean = {
    this.distanceSquared < that.distanceSquared
  }

  def <(that: ClosestPointWithType): Boolean = {
    this.distanceSquared < that.distanceSquared
  }
}

/**
 * The base type for all closest point types with additional information about the location of the point.
 * @param point The closest point location on the surface.
 * @param distanceSquared The squared distance to the closest point location.
 */
sealed abstract class ClosestPointWithType(val point: Point[_3D], val distanceSquared: Double) {

  def <(that: ClosestPointWithType): Boolean = {
    this.distanceSquared < that.distanceSquared
  }

  def distance = Math.sqrt(distanceSquared)
}

/**
 * The closest point is a vertex.
 * The additional information stored is the PointId of the vertex found.
 * @param pid PointId of the closest vertex.
 */
case class ClosestPointIsVertex(override val point: Point[_3D], override val distanceSquared: Double, pid: PointId)
    extends ClosestPointWithType(point, distanceSquared)

/**
 * The closest point lies on a line.
 * The additional information stored are the PointIds of the two end points of the line and the barycentric coordinate.
 * @param pids Tuple of PointIds of the two end points of the line.
 * @param bc The barycentric coordinates of the closest point location.
 */
case class ClosestPointOnLine(override val point: Point[_3D],
                              override val distanceSquared: Double,
                              pids: (PointId, PointId),
                              bc: Double)
    extends ClosestPointWithType(point, distanceSquared)

/**
 * The closest point is a vertex.
 * The additional information stored is the TriangleId and the barycentric coordinates of the point.
 * @param tid TriangleId of the triangle containing the closest point.
 * @param bc The barycentric coordinates of the closest point location.
 */
case class ClosestPointInTriangle(override val point: Point[_3D],
                                  override val distanceSquared: Double,
                                  tid: TriangleId,
                                  bc: BarycentricCoordinates)
    extends ClosestPointWithType(point, distanceSquared)

case class ClosestPointInTriangleOfTetrahedron(override val point: Point[_3D],
                                               override val distanceSquared: Double,
                                               tetId: TetrahedronId,
                                               triId: TriangleId,
                                               bc: BarycentricCoordinates)
    extends ClosestPointWithType(point, distanceSquared)

/**
 * The closest point is a vertex.
 * The additional information stored is the TriangleId and the barycentric coordinates of the point.
 * @param tid TriangleId of the tetrahedral containing the closest point.
 * @param bc The barycentric coordinates of the closest point location.
 */
case class ClosestPointInTetrahedron(override val point: Point[_3D],
                                     override val distanceSquared: Double,
                                     tid: TetrahedronId,
                                     bc: BarycentricCoordinates4)
    extends ClosestPointWithType(point, distanceSquared)
