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

import scalismo.common.FiniteDiscreteDomain.CanBound
import scalismo.geometry._
import scalismo.mesh.kdtree.KDTreeMap

trait Cell {
  def pointIds: IndexedSeq[Int]
}

trait DiscreteDomain[D <: Dim]  {
  def points: Iterator[Point[D]]
  def isDefinedAt(pt: Point[D]) : Boolean
}

trait PointGenerator[D <: Dim] extends Function0[Point[D]]

object DiscreteDomain {
  def fromPredicateAndGenerator[D <: Dim: NDSpace](generator: PointGenerator[D], _isDefinedAt: Point[D] => Boolean) = {
    new DiscreteDomain[D] {
      override def points = Iterator.continually(generator())
      override def isDefinedAt(pt: Point[D]) = _isDefinedAt(pt)
    }
  }
}

trait FiniteDiscreteDomain[D <: Dim] extends DiscreteDomain[D] with Equals { self =>


  def numberOfPoints: Int

  def pointId(pt : Point[D]) : Option[Int]
  def pointsWithId : Iterator[(Point[D], Int)] = points.zipWithIndex
  def transform(t : Point[D] => Point[D]) : FiniteDiscreteDomain[D] = {
    FiniteDiscreteDomain.fromSeq(self.points.map(t).toIndexedSeq)
  }


  /**
   * Returns the smallest continuous box domain that fully contains all the domain points.
   *
   * The bounding box is always oriented along the dimensions of the space (i.e. this method does not return rotated boxes)
   * */

  def boundingBox(implicit canBound : CanBound[D]) : BoxDomain[D] = {
    canBound.boundingBox(this)
  }



  override def equals(that : Any) = {
    that match {
      case d: SpatiallyIndexedFiniteDiscreteDomain[D] => d.canEqual(this) && points.toSeq == d.points.toSeq
      case _ => false
    }
  }

  override def canEqual(that : Any) = that.isInstanceOf[SpatiallyIndexedFiniteDiscreteDomain[D]]
  override def hashCode() = points.hashCode()

}

object FiniteDiscreteDomain {

  def fromSeq[D <: Dim](_points: IndexedSeq[Point[D]]) =
    new FiniteDiscreteDomain[D] {
      override def points = _points.toIterator

      override def isDefinedAt(p: Point[D]) = _points.contains(p)

      override def numberOfPoints = _points.size

      override def pointId(pt: Point[D]) = {
        val idx = _points.indexOf(pt)
        if (idx == -1) None else Some(idx)
      }
    }

  def fromPredicateAndGenerator[D <: Dim : NDSpace](generator: PointGenerator[D], _isDefinedAt: Point[D] => Boolean, _numberOfPoints: Int) = new FiniteDiscreteDomain[D] {
    override def points = Iterator.continually(generator()).take(_numberOfPoints)

    override def numberOfPoints = _numberOfPoints

    override def isDefinedAt(pt: Point[D]) = _isDefinedAt(pt) && points.contains(pt)

    override def pointId(pt: Point[D]) = {
      val idx = points.indexOf(pt)
      if (idx == -1) None else Some(idx)
    }
  }

  trait CanBound[D <: Dim] {
    def boundingBox(domain : FiniteDiscreteDomain[D]) : BoxDomain[D]
  }

  implicit object CanBound1D extends CanBound[_1D] {
    def boundingBox(domain: FiniteDiscreteDomain[_1D]): BoxDomain[_1D] = {
      val minx = domain.points.map(_(0)).min
      val maxx = domain.points.map(_(0)).max
      BoxDomain[_1D](Point(minx), Point(maxx))
    }
  }

  implicit object CanBound2D extends CanBound[_2D] {
      def boundingBox(domain : FiniteDiscreteDomain[_2D]) : BoxDomain[_2D] = {
        val minx = domain.points.map(_(0)).min
        val miny = domain.points.map(_(1)).min
        val maxx = domain.points.map(_(0)).max
        val maxy = domain.points.map(_(1)).max
        BoxDomain[_2D](Point(minx, miny), Point(maxx, maxy))
      }
    }

  implicit object CanBound3D extends CanBound[_3D] {
    def boundingBox(domain : FiniteDiscreteDomain[_3D]): BoxDomain[_3D] = {
      val minx = domain.points.map(_(0)).min
      val miny = domain.points.map(_(1)).min
      val minz = domain.points.map(_(2)).min
      val maxx = domain.points.map(_(0)).max
      val maxy = domain.points.map(_(1)).max
      val maxz = domain.points.map(_(2)).max
      BoxDomain[_3D](Point(minx, miny, minz), Point(maxx, maxy, maxz))
    }
  }

}


class SpatiallyIndexedFiniteDiscreteDomain[D <: Dim: NDSpace]  (private val pointSeq: IndexedSeq[Point[D]],
                                                                val numberOfPoints: Int) extends FiniteDiscreteDomain[D] {

  override def points = pointSeq.toIterator
  def points(id : Int) = pointSeq(id)
  def apply(id : Int) = points(id)

  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(pointSeq.zipWithIndex)
  override def isDefinedAt(pt: Point[D]) = findClosestPoint(pt)._1 == pt

  def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    nearestPtsAndIndices(0)
  }

  def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] = kdTreeMap.findNearest(pt, n)

  override def pointId(pt : Point[D]) = {
    val idx = pointSeq.indexOf(pt)
    if (idx == -1) None else Some(idx)
  }

}

object SpatiallyIndexedFiniteDiscreteDomain {

  def apply[D <: Dim : NDSpace](pointSeq: IndexedSeq[Point[D]], numberOfPoints: Int) = {
    new SpatiallyIndexedFiniteDiscreteDomain[D](pointSeq, numberOfPoints)
  }

  def fromSeq[D <: Dim: NDSpace](_points: IndexedSeq[Point[D]]) = SpatiallyIndexedFiniteDiscreteDomain[D](_points, _points.size)
  def fromGenereator[D <: Dim: NDSpace](generator: PointGenerator[D], _numberOfPoitns: Int) = {
    val _points = Iterator.continually(generator()).take(_numberOfPoitns).toIndexedSeq
    SpatiallyIndexedFiniteDiscreteDomain[D](_points, _numberOfPoitns)
  } 

}


