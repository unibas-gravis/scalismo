package org.statismo.stk.core
package common

import scala.collection.SeqView
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.mesh.kdtree.KDTreeMap


trait Cell { 
  def pointIds : IndexedSeq[Int]
}

abstract class DiscreteDomain[D <: Dim : NDSpaceOps] extends Domain[D]{


  def dimensionality: Int = implicitly[NDSpaceOps[D]].dimensionality

  def points: SeqView[Point[D],Seq[_]]
  // def cells : IndexedSeq[Cell] // TODO add it later here

  def numberOfPoints: Int = points.size

  //def neighbors(pt: Dim): IndexedSeq[Dim]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size



}


abstract class UnstructuredPointsDomainBase[D <: Dim : NDSpaceOps](_points : IndexedSeq[Point[D]]) extends DiscreteDomain[D] {


  private[this] lazy val kdTreeMap = KDTreeMap.fromSeq(points.zipWithIndex.toIndexedSeq)
  override def points = _points.view


  def isDefinedAt(pt: Point[D]) = {
    val (closestPt, _) = findClosestPoint(pt)
    closestPt == pt
  }


  def findClosestPoint(pt: Point[D]): (Point[D], Int) = {
    val nearestPtsAndIndices = (kdTreeMap.findNearest(pt, n = 1))
    nearestPtsAndIndices(0)
  }

  def findNClosestPoints(pt: Point[D], n: Int): Seq[(Point[D], Int)] = {
    kdTreeMap.findNearest(pt, n)
  }

}
case class UnstructuredPointsDomain[D <: Dim : NDSpaceOps](_points : IndexedSeq[Point[D]]) extends UnstructuredPointsDomainBase[D](_points)
