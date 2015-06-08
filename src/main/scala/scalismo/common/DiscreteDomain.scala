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
import scalismo.image.{ DiscreteImageDomain, DiscreteScalarImage }
import scalismo.mesh.kdtree.KDTreeMap

import scala.reflect.ClassTag

trait Cell {
  def pointIds: IndexedSeq[Int]
}

trait PointGenerator[D <: Dim] extends Function0[Point[D]]

trait DiscreteDomain[D <: Dim] extends Equals { self =>

  def numberOfPoints: Int
  def points: Iterator[Point[D]]
  def pointIds: Iterator[Int] = Iterator.range(0, numberOfPoints)
  def isDefinedAt(pt: Point[D]): Boolean
  def pointId(pt: Point[D]): Option[Int]
  def pointsWithId: Iterator[(Point[D], Int)] = points.zipWithIndex

  def point(id: Int): Point[D]

  def findClosestPoint(pt: Point[D]): (Point[D], Int)
  def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)]

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

