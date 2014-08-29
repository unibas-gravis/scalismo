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
  def points: Iterator[Point[D]]
}

class DiscreteDomain2[D <: Dim]private (points : Iterator[Point[D]])

trait PointGenerator[D <: Dim] extends Function0[Point[D]]

object DiscreteDomain {
  def fromPredicateAndGenerator[D <: Dim: DimOps](generator: PointGenerator[D], _isDefinedAt: Point[D] => Boolean) = {
    new DiscreteDomain[D] {
      override def myPoints = Iterator.continually(generator())
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
      override def myPoints = _points.toIterator
      override def isDefinedAt(p: Point[D]) = _points.contains(p)
      override def numberOfPoints = _points.size
    }

  def fromPredicateAndGenerator[D <: Dim: DimOps](generator: PointGenerator[D], _isDefinedAt: Point[D] => Boolean, _numberOfPoints: Int) = new FiniteDiscreteDomain[D] {
    override def myPoints = Iterator.continually(generator()).take(_numberOfPoints)
    override def numberOfPoints = _numberOfPoints
    override def isDefinedAt(pt: Point[D]) = _isDefinedAt(pt) && myPoints.contains(pt)
  }
}

trait SpatiallyIndexedFiniteDiscreteDomain[D <: Dim] extends FiniteDiscreteDomain[D] {

  def dimOps : DimOps[D]
  implicit val d = dimOps
  
  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(points.toIndexedSeq.zipWithIndex) 
  override def isDefinedAt(pt: Point[D]) =  findClosestPoint(pt)._1 == pt
  
  def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    nearestPtsAndIndices(0)
  }

  def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] =  kdTreeMap.findNearest(pt, n)
}

object SpatiallyIndexedFiniteDiscreteDomain {
  
  def fromSeq[D <: Dim: DimOps](_points: IndexedSeq[Point[D]]) = new SpatiallyIndexedFiniteDiscreteDomain[D] {  
  //  override def dimOps = implicitly[DimOps[D]]
    override def myPoints = _points.toIterator
    override def numberOfPoints = _points.size
  }
  
  def fromGenereator[D <: Dim : DimOps](generator : PointGenerator[D], _numberOfPoitns : Int) = new SpatiallyIndexedFiniteDiscreteDomain[D] {  
    override def myPoints =  Iterator.continually(generator()).take(_numberOfPoitns)
    override def numberOfPoints = _numberOfPoitns
  }
}


