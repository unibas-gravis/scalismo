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

import scalismo.common.UnstructuredPointsDomain.Create
import scalismo.geometry._
import scalismo.mesh.kdtree.{ KDTreeMap, RegionBuilder }
import scala.collection.parallel.CollectionConverters._
import scala.language.implicitConversions

sealed abstract class UnstructuredPointsDomain[D: NDSpace: Create] private[scalismo] (private[scalismo] val pointSequence: IndexedSeq[Point[D]]) extends DiscreteDomain[D] {

  override def points: Iterator[Point[D]] = pointSequence.toIterator
  override def numberOfPoints = pointSequence.size

  override def point(id: PointId) = pointSequence(id.id)

  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(pointSequence.zipWithIndex)
  private[this] lazy val pointIDMap = pointSequence.zipWithIndex.map { case (pt, id) => (pt, PointId(id)) }.toMap

  override def isDefinedAt(pt: Point[D]) = pointIDMap.contains(pt)

  override def findClosestPoint(pt: Point[D]): PointWithId[D] = {

    def kdtreeLookup(pt: Point[D]): PointWithId[D] = {
      val (nearestPt, nearestInd) = kdTreeMap.findNearest(pt, n = 1).head
      PointWithId(nearestPt, PointId(nearestInd))
    }

    // first we check if the point is part of the domain (i.e. we get a pointId for the point).
    // if not, we do a KDtree lookup, which is more expensive
    pointId(pt) match {
      case Some(id) => PointWithId(pt, id)
      case None => kdtreeLookup(pt)
    }
  }

  def findPointsInRegion(region: BoxDomain[D]): Seq[PointWithId[D]] = {

    val dim = implicitly[NDSpace[D]]
    val regionBuilder = new RegionBuilder[Point[D]]
    val a = region.origin
    val b = region.origin + region.extent
    val reg = (0 until dim.dimensionality).foldLeft[RegionBuilder[Point[D]]](regionBuilder) { case (rg, dim) => rg.from(a, dim).to(b, dim) }
    kdTreeMap.regionQuery(reg).map { case (p, id) => PointWithId(p, PointId(id)) }

  }

  override def findNClosestPoints(pt: Point[D], n: Int): Seq[PointWithId[D]] = {
    kdTreeMap.findNearest(pt, n).map { case (p, id) => PointWithId(p, PointId(id)) }
  }

  override def pointId(pt: Point[D]): Option[PointId] = {
    pointIDMap.get(pt)
  }

  override def transform(transform: Point[D] => Point[D]): UnstructuredPointsDomain[D] = {
    UnstructuredPointsDomain(pointSequence.par.map(transform).toIndexedSeq)
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

  def apply[D: NDSpace](points: IndexedSeq[Point[D]])(implicit creator: Create[D]): UnstructuredPointsDomain[D] = {
    creator.create(points)
  }

  def fromGenerator[D](generator: PointGenerator[D], numberOfPoints: Int)(implicit creator: Create[D]) = {
    val points = Iterator.continually(generator()).take(numberOfPoints).toIndexedSeq
    creator.create(points)
  }

  trait Create[D] {
    def create(points: IndexedSeq[Point[D]]): UnstructuredPointsDomain[D]
  }

  object Create {
    implicit object CreateUnstructuredPointsDomain1D extends Create[_1D] {
      override def create(points: IndexedSeq[Point[_1D]]) = new UnstructuredPointsDomain1D(points)
    }
    implicit object CreateUnstructuredPointsDomain2D extends Create[_2D] {
      override def create(points: IndexedSeq[Point[_2D]]) = new UnstructuredPointsDomain2D(points)
    }
    implicit object CreateUnstructuredPointsDomain3D extends Create[_3D] {
      override def create(points: IndexedSeq[Point[_3D]]) = new UnstructuredPointsDomain3D(points)
    }

  }

}

class UnstructuredPointsDomain1D private[scalismo] (pointSequence: IndexedSeq[Point[_1D]]) extends UnstructuredPointsDomain[_1D](pointSequence) {

  override def boundingBox: BoxDomain[_1D] = {
    val minx = pointSequence.map(_(0)).min
    val maxx = pointSequence.map(_(0)).max
    BoxDomain(Point(minx), Point(maxx))
  }

  override def transform(t: Point[_1D] => Point[_1D]): UnstructuredPointsDomain1D = {
    new UnstructuredPointsDomain1D(pointSequence.map(t).toIndexedSeq)
  }

}

class UnstructuredPointsDomain2D private[scalismo] (pointSequence: IndexedSeq[Point[_2D]]) extends UnstructuredPointsDomain[_2D](pointSequence) {

  override def boundingBox: BoxDomain[_2D] = {
    val minx = pointSequence.map(_(0)).min
    val miny = pointSequence.map(_(1)).min
    val maxx = pointSequence.map(_(0)).max
    val maxy = pointSequence.map(_(1)).max
    BoxDomain(Point(minx, miny), Point(maxx, maxy))
  }

  override def transform(t: Point[_2D] => Point[_2D]): UnstructuredPointsDomain2D = {
    new UnstructuredPointsDomain2D(pointSequence.map(t).toIndexedSeq)
  }

}

class UnstructuredPointsDomain3D private[scalismo] (pointSequence: IndexedSeq[Point[_3D]]) extends UnstructuredPointsDomain[_3D](pointSequence) {

  override def boundingBox: BoxDomain[_3D] = {
    val minx = pointSequence.map(_(0)).min
    val miny = pointSequence.map(_(1)).min
    val minz = pointSequence.map(_(2)).min
    val maxx = pointSequence.map(_(0)).max
    val maxy = pointSequence.map(_(1)).max
    val maxz = pointSequence.map(_(2)).max
    BoxDomain(Point(minx, miny, minz), Point(maxx, maxy, maxz))
  }

  override def transform(t: Point[_3D] => Point[_3D]): UnstructuredPointsDomain3D = {
    new UnstructuredPointsDomain3D(pointSequence.map(t).toIndexedSeq)
  }

}

