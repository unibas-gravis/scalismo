package org.statismo.stk.core
package common

import scala.collection.SeqView
import geometry.{Point, Vector, Dim, OneD, TwoD, ThreeD}


trait Cell { 
  def pointIds : IndexedSeq[Int]
}

trait DiscreteDomain[D <: Dim] extends Domain[D]{
  def dimensionality: Int
  def points: SeqView[Point[D],Seq[_]]
  // def cells : IndexedSeq[Cell] // TODO add it later here
  def numberOfPoints: Int

  //def neighbors(pt: Dim): IndexedSeq[Dim]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}
