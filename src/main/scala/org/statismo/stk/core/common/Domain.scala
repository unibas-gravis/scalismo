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


trait BoxedDomain[D <: Dim] extends Domain[D] {
  def origin: Point[D]
  def extent: Point[D]
  def volume: Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (extent(i) - origin(i)))
  def isDefinedAt(pt: Point[D]): Boolean = {
    def isInsideAxis(i: Int) = pt(i) >= origin(i) && pt(i) <= extent(i)
    (0 until pt.dimensionality).foldLeft(true)((defined, i) => defined && isInsideAxis(i))
  }
}


object BoxedDomain {
  
  def apply[D <: Dim](originV: Point[D], extentV: Point[D]) = new BoxedDomain[D]{
    override def origin = originV
    override def extent = extentV   
  }
}

