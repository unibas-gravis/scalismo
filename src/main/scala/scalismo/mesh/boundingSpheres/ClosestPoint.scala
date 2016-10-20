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

class ClosestPoint(val point: Point[_3D],
    val distance2: Double) {
  def <(that: ClosestPoint) = {
    this.distance2 < that.distance2
  }

  def toPoint() = point
}

case class ClosestPointIsPoint(override val point: Point[_3D],
  override val distance2: Double,
  idx: PointId) extends ClosestPoint(point, distance2)

case class ClosestPointOnLine(override val point: Point[_3D],
  override val distance2: Double,
  idx: (PointId, PointId),
  bc: Double) extends ClosestPoint(point, distance2)

case class ClosestPointInTriangle(override val point: Point[_3D],
  override val distance2: Double,
  idx: TriangleId,
  bc: BarycentricCoordinates) extends ClosestPoint(point, distance2)
