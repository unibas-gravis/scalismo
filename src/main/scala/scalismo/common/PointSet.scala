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
final case class PointWithId[D](point: Point[D], id: PointId)

trait Cell {
  def pointIds: IndexedSeq[PointId]
}

trait PointGenerator[D] extends Function0[Point[D]]

trait PointSet[D] extends Equals {
  self =>

  def numberOfPoints: Int

  def points: Iterator[Point[D]]

  /**
   * *
   * Returns the domain points in n chunks. Each chunk of the points is given as an iterator
   *
   * The main idea behind this method is to be able to easily parallelize on the domain points, as parallel operations
   * on a single iterator in Scala end up more costly than sequential access in our case. Using this method, one would parallelize on the
   * Seq of iterators instead.
   *
   */
  def pointsInChunks(nChunks: Int): Seq[Iterator[Point[D]]]

  def pointIds: Iterator[PointId] = Iterator.range(0, numberOfPoints).map(id => PointId(id))

  def isDefinedAt(pt: Point[D]): Boolean

  def pointId(pt: Point[D]): Option[PointId]

  def pointsWithId: Iterator[(Point[D], PointId)] = points.zipWithIndex.map { case (pt, id) => (pt, PointId(id)) }

  def point(id: PointId): Point[D]

  /**
   * *
   * Returns the point belonging to the domain that is closest to the indicated position. The point identifier
   * within th e domain is also returned.
   */
  def findClosestPoint(pt: Point[D]): PointWithId[D]

  def findNClosestPoints(pt: Point[D], n: Int): Seq[PointWithId[D]]

  def transform(t: Point[D] => Point[D]): PointSet[D]

  /**
   * Returns the smallest continuous box domain that fully contains all the domain points.
   *
   * The bounding box is always oriented along the dimensions of the space (i.e. this method does not return rotated boxes)
   */
  def boundingBox: BoxDomain[D]

  override def equals(that: Any) = {
    that match {
      case d: PointSet[D @unchecked] => d.canEqual(this) && points.toSeq == d.points.toSeq
      case _                         => false
    }
  }

  override def canEqual(that: Any) = that.isInstanceOf[PointSet[D @unchecked]]

  override def hashCode() = points.toSeq.hashCode()

}
