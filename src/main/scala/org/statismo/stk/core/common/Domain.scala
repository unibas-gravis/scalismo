package org.statismo.stk.core
package common

import geometry.{ Point, Vector, Dim, _1D, _2D, _3D }
import breeze.linalg.DenseVector

trait Domain[D <: Dim] {
  def isDefinedAt(pt: Point[D]): Boolean
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

object RealSpace1D extends Domain[_1D]{ override def isDefinedAt(pt: Point[_1D])= true}
object RealSpace2D extends Domain[_2D]{ override def isDefinedAt(pt: Point[_2D])= true}
object RealSpace3D extends Domain[_3D]{ override def isDefinedAt(pt: Point[_3D])= true}


trait BoxLike[D <: Dim] {
  def origin: Point[D]
  def corner: Point[D]
  def extent : Vector[D] = corner - origin
  def volume: Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (corner(i) - origin(i)))
  def isInside(pt : Point[D]) : Boolean = {
    def isInsideAxis(i: Int) = pt(i) >= origin(i) && pt(i) <= corner(i)
    (0 until pt.dimensionality).foldLeft(true)((defined, i) => defined && isInsideAxis(i))
  }

}

case class BoxDomain[D <: Dim](origin : Point[D], corner : Point[D]) extends Domain[D] with BoxLike[D] {
  def isDefinedAt(pt: Point[D]): Boolean = {
    isInside(pt)
  }
}

