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
import scalismo.mesh.kdtree.KDTreeMap
import scala.language.implicitConversions

sealed abstract class UnstructuredPointsDomain[D <: Dim: NDSpace] private[scalismo] (pointSeq: IndexedSeq[Point[D]]) extends DiscreteDomain[D] {

  override def points: Iterator[Point[D]] = pointSeq.toIterator
  override def numberOfPoints = points.size

  override def point(id: PointId) = pointSeq(id.id)

  private[this] val kdTreeMap = KDTreeMap.fromSeq(pointSeq.zipWithIndex)
  private[this] val pointIDMap = pointSeq.zipWithIndex.map { case (pt, id) => (pt, PointId(id)) }.toMap

  override def isDefinedAt(pt: Point[D]) = pointIDMap.contains(pt)

  override def findClosestPoint(pt: Point[D]): (Point[D], PointId) = {

    def kdtreeLookup(pt: Point[D]): (Point[D], PointId) = {
      val (nearestPt, nearestInd) = kdTreeMap.findNearest(pt, n = 1).head
      (nearestPt, PointId(nearestInd))
    }

    // first we check if the point is part of the domain (i.e. we get a pointId for the point).
    // if not, we do a KDtree lookup, which is more expensive
    pointId(pt) match {
      case Some(id) => (pt, id)
      case None => kdtreeLookup(pt)
    }
  }

  override def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], PointId)] = {
    kdTreeMap.findNearest(pt, n).map { case (p, id) => (p, PointId(id)) }
  }

  override def pointId(pt: Point[D]): Option[PointId] = {
    pointIDMap.get(pt)
  }

}

object UnstructuredPointsDomain {

  implicit def parametricToConcreteType1D(unstructuredPointsDomain: UnstructuredPointsDomain[_1D]): UnstructuredPointsDomain1D = {
    unstructuredPointsDomain.asInstanceOf[UnstructuredPointsDomain1D]
  }

  implicit def parametricToConcreteType2D(unstructuredPointsDomain: UnstructuredPointsDomain[_2D]): UnstructuredPointsDomain2D = {
    unstructuredPointsDomain.asInstanceOf[UnstructuredPointsDomain2D]
  }

  implicit def parametricToConcreteType3D(unstructuredPointsDomain: UnstructuredPointsDomain[_3D]): UnstructuredPointsDomain3D = {
    unstructuredPointsDomain.asInstanceOf[UnstructuredPointsDomain3D]
  }

  def apply[D <: Dim](points: IndexedSeq[Point[D]])(implicit creator: CreateUnstructuredPointsDomain[D]) = {
    creator.create(points)
  }

  def fromGenerator[D <: Dim](generator: PointGenerator[D], numberOfPoints: Int)(implicit creator: CreateUnstructuredPointsDomain[D]) = {
    val points = Iterator.continually(generator()).take(numberOfPoints).toIndexedSeq
    creator.create(points)
  }
}

class UnstructuredPointsDomain1D private[scalismo] (pointSeq: IndexedSeq[Point[_1D]]) extends UnstructuredPointsDomain[_1D](pointSeq) {

  override def boundingBox: BoxDomain[_1D] = {
    val minx = pointSeq.map(_(0)).min
    val maxx = pointSeq.map(_(0)).max
    BoxDomain[_1D](Point(minx), Point(maxx))
  }

  override def transform(t: Point[_1D] => Point[_1D]): UnstructuredPointsDomain1D = {
    new UnstructuredPointsDomain1D(pointSeq.map(t).toIndexedSeq)
  }

}

class UnstructuredPointsDomain2D private[scalismo] (pointSeq: IndexedSeq[Point[_2D]]) extends UnstructuredPointsDomain[_2D](pointSeq) {

  override def boundingBox: BoxDomain[_2D] = {
    val minx = pointSeq.map(_(0)).min
    val miny = pointSeq.map(_(1)).min
    val maxx = pointSeq.map(_(0)).max
    val maxy = pointSeq.map(_(1)).max
    BoxDomain[_2D](Point(minx, miny), Point(maxx, maxy))
  }

  override def transform(t: Point[_2D] => Point[_2D]): UnstructuredPointsDomain2D = {
    new UnstructuredPointsDomain2D(pointSeq.map(t).toIndexedSeq)
  }

}

class UnstructuredPointsDomain3D private[scalismo] (pointSeq: IndexedSeq[Point[_3D]]) extends UnstructuredPointsDomain[_3D](pointSeq) {

  override def boundingBox: BoxDomain[_3D] = {
    val minx = pointSeq.map(_(0)).min
    val miny = pointSeq.map(_(1)).min
    val minz = pointSeq.map(_(2)).min
    val maxx = pointSeq.map(_(0)).max
    val maxy = pointSeq.map(_(1)).max
    val maxz = pointSeq.map(_(2)).max
    BoxDomain[_3D](Point(minx, miny, minz), Point(maxx, maxy, maxz))
  }

  override def transform(t: Point[_3D] => Point[_3D]): UnstructuredPointsDomain3D = {
    new UnstructuredPointsDomain3D(pointSeq.map(t).toIndexedSeq)
  }

}

trait CreateUnstructuredPointsDomain[D <: Dim] {
  def create(points: IndexedSeq[Point[D]]): UnstructuredPointsDomain[D]
}

object CreateUnstructuredPointsDomain {
  implicit object CreateUnstructuredPointsDomain1D extends CreateUnstructuredPointsDomain[_1D] {
    override def create(points: IndexedSeq[Point[_1D]]) = new UnstructuredPointsDomain1D(points)
  }
  implicit object CreateUnstructuredPointsDomain2D extends CreateUnstructuredPointsDomain[_2D] {
    override def create(points: IndexedSeq[Point[_2D]]) = new UnstructuredPointsDomain2D(points)
  }
  implicit object CreateUnstructuredPointsDomain3D extends CreateUnstructuredPointsDomain[_3D] {
    override def create(points: IndexedSeq[Point[_3D]]) = new UnstructuredPointsDomain3D(points)
  }

}
