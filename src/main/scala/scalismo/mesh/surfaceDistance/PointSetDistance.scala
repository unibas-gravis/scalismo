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
package scalismo.mesh.surfaceDistance

import breeze.numerics._
import BSDistance.{Distance2, Index, _}
import scalismo.geometry.{Point, Vector, _3D}


case class PointSetDistance(private val bs: BoundingSphere,
                            private val points: Seq[Point[_3D]]) {
  val pointList = points.map(_.toVector).toIndexedSeq
  var lastIdx: Int = 0


  def closestPoint(point: Point[_3D]): (Double, Point[_3D], Int) = {
    val p = point.toVector
    val index = Index(lastIdx)
    val lastP = pointList(lastIdx)
    val lastD = toPoint(lastP, p)
    val d: Distance2 = new Distance2(lastD.distance2)
    distanceToPartition(p, bs, d, index)
    (d.distance2, pointList(index.idx).toPoint, index.idx)
  }

  def distanceToPartition(point: Vector[_3D],
                          partition: BoundingSphere,
                          result: Distance2,
                          index: Index): Unit = {
    if (partition.idx >= 0) {
      // we have found a leave
      val res = BSDistance.toPoint(point, pointList(partition.idx))
      if (res.distance2 < result.distance2) {
        result.distance2 = res.distance2
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

}

object PointSetDistance {
  implicit def closestPointDistance(ps: PointSetDistance, point: Vector[_3D], idx: Int): Distance2 = {
    BSDistance.toPoint(point, ps.pointList(idx))
  }
}

