package org.statismo.stk.core
package common

import geometry.{ Point, Vector, Dim, _1D, _2D, _3D }
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.{CanInvert, Transformation}

trait Domain[D <: Dim] { self =>
  def isDefinedAt(pt: Point[D]): Boolean

  def warp(t : Transformation[D] with CanInvert[D]) : Domain[D] = new Domain[D] {
    val tinv = t.inverse
    override def isDefinedAt(pt : Point[D]): Boolean = {
      self.isDefinedAt(tinv(pt))
    }
  }
}

object Domain {
  def fromPredicate[D <: Dim](chi : Point[D] => Boolean) = new Domain[D] {
    override def isDefinedAt(pt: Point[D]) = chi(pt)
  }
  
  def intersection[D <: Dim](thisDomain: Domain[D], thatDomain: Domain[D]) = new Domain[D] {
    override def isDefinedAt(pt: Point[D]) = thisDomain.isDefinedAt(pt) && thatDomain.isDefinedAt(pt)
  }

  def union[D <: Dim](thisDomain: Domain[D], thatDomain: Domain[D]) = new Domain[D] {
    override def isDefinedAt(pt: Point[D]) = thisDomain.isDefinedAt(pt) || thatDomain.isDefinedAt(pt)
  }
}

class RealSpace[D <: Dim] extends Domain[D] {
  override def isDefinedAt(pt: Point[D])= true
}
object RealSpace {
  def apply[D <: Dim] = new RealSpace[D]
}


case class BoxDomain[D <: Dim](origin : Point[D], oppositeCorner : Point[D]) extends Domain[D]  {
  def isDefinedAt(pt: Point[D]): Boolean = {
    isInside(pt)
  }

  def extent : Vector[D] = oppositeCorner - origin
  def volume: Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (oppositeCorner(i) - origin(i)))
  def isInside(pt : Point[D]) : Boolean = {
    def isInsideAxis(i: Int) = pt(i) >= origin(i) && pt(i) <= oppositeCorner(i)
    (0 until pt.dimensionality).foldLeft(true)((defined, i) => defined && isInsideAxis(i))
  }
}

