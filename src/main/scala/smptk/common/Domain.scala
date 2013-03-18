package smptk
package common


import smptk.image.Geometry._
import smptk.image.CoordVector


trait Domain[CV[A] <: CoordVector[A]] {
  def dimensionality: Int
}

trait Cell { 
  def pointIds : IndexedSeq[Int]
}


trait DiscreteDomain[CV[A] <: CoordVector[A]] extends Domain[CV] {
  def points: IndexedSeq[CV[Double]]
  // def cells : IndexedSeq[Cell] // TODO add it later here
  def numberOfPoints: Int

  //def neighbors(pt: CoordVector): IndexedSeq[CoordVector]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}


trait ContinuousDomain[CV[A] <: CoordVector[A]] extends Domain[CV] {
}

