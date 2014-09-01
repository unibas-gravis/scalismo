package org.statismo.stk.core
package common

import scala.collection.SeqView
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.mesh.kdtree.KDTreeMap
import com.sun.xml.internal.bind.annotation.OverrideAnnotationOf

trait Cell {
  def pointIds: IndexedSeq[Int]
}

trait DiscreteDomain[D <: Dim] extends Domain[D] {
  def points: Stream[Point[D]]
}

trait PointGenerator[D <: Dim] extends Function0[Point[D]]

object DiscreteDomain {
  def fromPredicateAndGenerator[D <: Dim: DimOps](generator: PointGenerator[D], _isDefinedAt: Point[D] => Boolean) = {
    new DiscreteDomain[D] {
      override def points = Stream.continually(generator())
      override def isDefinedAt(pt: Point[D]) = _isDefinedAt(pt)
    }
  }
}

trait FiniteDiscreteDomain[D <: Dim] extends DiscreteDomain[D] {
  def numberOfPoints: Int
}

object FiniteDiscreteDomain {

  def fromSeq[D <: Dim: DimOps](_points: IndexedSeq[Point[D]]) =
    new FiniteDiscreteDomain[D] {
      override def points = _points.toStream
      override def isDefinedAt(p: Point[D]) = _points.contains(p)
      override def numberOfPoints = _points.size
    }

  def fromPredicateAndGenerator[D <: Dim: DimOps](generator: PointGenerator[D], _isDefinedAt: Point[D] => Boolean, _numberOfPoints: Int) = new FiniteDiscreteDomain[D] {
    override def points = Stream.continually(generator()).take(_numberOfPoints)
    override def numberOfPoints = _numberOfPoints
    override def isDefinedAt(pt: Point[D]) = _isDefinedAt(pt) && points.contains(pt)
  }
}

case class SpatiallyIndexedFiniteDiscreteDomain[D <: Dim: DimOps]  (points: Stream[Point[D]], numberOfPoints: Int) extends FiniteDiscreteDomain[D] {

  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(points.toIndexedSeq.zipWithIndex)
  override def isDefinedAt(pt: Point[D]) = findClosestPoint(pt)._1 == pt

  def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    nearestPtsAndIndices(0)
  }

  def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] = kdTreeMap.findNearest(pt, n)
}

object SpatiallyIndexedFiniteDiscreteDomain {
  def fromSeq[D <: Dim: DimOps](_points: IndexedSeq[Point[D]]) = SpatiallyIndexedFiniteDiscreteDomain[D](_points.toStream, _points.size)
  def fromGenereator[D <: Dim: DimOps](generator: PointGenerator[D], _numberOfPoitns: Int) = SpatiallyIndexedFiniteDiscreteDomain[D](Stream.continually(generator()).take(_numberOfPoitns), _numberOfPoitns)

}


