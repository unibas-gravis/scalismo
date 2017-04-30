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
import scalismo.geometry.{ Point, _3D }
import scalismo.mesh.{ BarycentricCoordinates, TriangleId }

sealed class ClosestPoint(val point: Point[_3D],
    val distanceSquared: Double) {
  def <(that: ClosestPoint) = {
    this.distanceSquared < that.distanceSquared
  }

  def toPoint() = point
}

case class ClosestPointIsVertex(override val point: Point[_3D],
  override val distanceSquared: Double,
  idx: PointId) extends ClosestPoint(point, distanceSquared)

case class ClosestPointOnLine(override val point: Point[_3D],
  override val distanceSquared: Double,
  idx: (PointId, PointId),
  bc: Double) extends ClosestPoint(point, distanceSquared)

case class ClosestPointInTriangle(override val point: Point[_3D],
  override val distanceSquared: Double,
  idx: TriangleId,
  bc: BarycentricCoordinates) extends ClosestPoint(point, distanceSquared)
