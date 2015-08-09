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
package scalismo.common

import scalismo.geometry._

final case class PointId(id: Int) extends AnyVal

trait Cell {
  def pointIds: IndexedSeq[PointId]
}

trait PointGenerator[D <: Dim] extends Function0[Point[D]]

trait DiscreteDomain[D <: Dim] extends Equals {
  self =>

  def numberOfPoints: Int

  def points: Iterator[Point[D]]

  def pointIds: Iterator[PointId] = Iterator.range(0, numberOfPoints).map(id => PointId(id))

  def isDefinedAt(pt: Point[D]): Boolean

  def pointId(pt: Point[D]): Option[PointId]

  def pointsWithId: Iterator[(Point[D], PointId)] = points.zipWithIndex.map { case (pt, id) => (pt, PointId(id)) }

  def point(id: PointId): Point[D]

  def findClosestPoint(pt: Point[D]): (Point[D], PointId)

  def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], PointId)]

  def transform(t: Point[D] => Point[D]): DiscreteDomain[D]

  /**
   * Returns the smallest continuous box domain that fully contains all the domain points.
   *
   * The bounding box is always oriented along the dimensions of the space (i.e. this method does not return rotated boxes)
   */
  def boundingBox: BoxDomain[D]

  override def equals(that: Any) = {
    that match {
      case d: DiscreteDomain[D] => d.canEqual(this) && points.toSeq == d.points.toSeq
      case _ => false
    }
  }

  override def canEqual(that: Any) = that.isInstanceOf[DiscreteDomain[D]]

  override def hashCode() = points.hashCode()

}

