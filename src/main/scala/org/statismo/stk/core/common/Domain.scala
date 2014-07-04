package org.statismo.stk.core
package common

import geometry.{Point, Vector, Dim, _1D, _2D, _3D}
import breeze.linalg.DenseVector



trait Domain[D <: Dim] { self =>
  def isDefinedAt(pt : Point[D]) : Boolean
  def intersection(that : Domain[D]) = new ImplicitDomain[D] {
    override val chi = (pt : Point[D]) =>  self.isDefinedAt(pt) && that.isDefinedAt(pt)
  }
  def union(that : Domain[D]) = new ImplicitDomain[D] {
    override val chi = (pt : Point[D]) =>  self.isDefinedAt(pt) || that.isDefinedAt(pt)
  }

}

trait RealSpace[D <: Dim] extends Domain[D] {
  def isDefinedAt(pt : Point[D]) = true
}
object RealSpace1D extends RealSpace[_1D]
object RealSpace2D extends RealSpace[_2D]
object RealSpace3D extends RealSpace[_3D]

/**
 * A region defined implicitly by an indicator function
 * 
 * @param chi : The indicator function
 */
trait ImplicitDomain[D <: Dim] extends Domain[D] {
  val chi : Point[D] =>  Boolean
  override def isDefinedAt(pt : Point[D]) : Boolean = chi(pt)
}

case class ImplicitDomain1D(val chi : Point[_1D] => Boolean) extends ImplicitDomain[_1D] {}
case class ImplicitDomain2D(val chi : Point[_2D] => Boolean) extends ImplicitDomain[_2D] {}
case class ImplicitDomain3D(val chi : Point[_3D] => Boolean) extends ImplicitDomain[_3D] {}


trait BoxedDomain[D <: Dim] extends Domain[D] {
  def origin: Point[D]
  def extent: Point[D]
  def volume : Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (extent(i) - origin(i)))
  def isDefinedAt(pt : Point[D]) : Boolean = {
    def isInsideAxis(i : Int) = pt(i) >= origin(i) &&  pt(i) <= extent(i)    
    (0 until pt.dimensionality).foldLeft(true)((defined,i) => defined && isInsideAxis(i))
  }
}

case class BoxedDomain1D( originV : Point[_1D], extentV: Point[_1D]) extends BoxedDomain[_1D]  {
  def dimensionality = 1
  def origin = originV
  def extent = extentV
}

case class BoxedDomain2D(originV : Point[_2D], extentV: Point[_2D]) extends BoxedDomain[_2D]{
  def dimensionality = 2
  def origin = originV
  def extent = extentV
}

case class BoxedDomain3D(originV : Point[_3D], extentV: Point[_3D]) extends BoxedDomain[_3D] {
  def dimensionality = 3
  def origin = originV
  def extent = extentV  
}

