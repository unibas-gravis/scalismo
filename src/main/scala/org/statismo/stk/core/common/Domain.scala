package org.statismo.stk.core
package common

import geometry.{Point, Vector, Dim, OneD, TwoD, ThreeD}
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
object RealSpace1D extends RealSpace[OneD] 
object RealSpace2D extends RealSpace[TwoD] 
object RealSpace3D extends RealSpace[ThreeD]

/**
 * A region defined implicitly by an indicator function
 * 
 * @param chi : The indicator function
 */
trait ImplicitDomain[D <: Dim] extends Domain[D] {
  val chi : Point[D] =>  Boolean
  override def isDefinedAt(pt : Point[D]) : Boolean = chi(pt)
}

case class ImplicitDomain1D(val chi : Point[OneD] => Boolean) extends ImplicitDomain[OneD] {}
case class ImplicitDomain2D(val chi : Point[TwoD] => Boolean) extends ImplicitDomain[TwoD] {}
case class ImplicitDomain3D(val chi : Point[ThreeD] => Boolean) extends ImplicitDomain[ThreeD] {}


trait BoxedDomain[D <: Dim] extends Domain[D] {
  def origin: Point[D]
  def extent: Point[D]
  def volume : Double = (0 until origin.dimensionality).foldLeft(1.0)((prod, i) => prod * (extent(i) - origin(i)))
  def isDefinedAt(pt : Point[D]) : Boolean = {
    def isInsideAxis(i : Int) = pt(i) >= origin(i) &&  pt(i) <= extent(i)    
    (0 until pt.dimensionality).foldLeft(true)((defined,i) => defined && isInsideAxis(i))
  }
}

case class BoxedDomain1D( originV : Point[OneD], extentV: Point[OneD]) extends BoxedDomain[OneD]  {
  def dimensionality = 1
  def origin = originV
  def extent = extentV
}

case class BoxedDomain2D(originV : Point[TwoD], extentV: Point[TwoD]) extends BoxedDomain[TwoD]{
  def dimensionality = 2
  def origin = originV
  def extent = extentV
}

case class BoxedDomain3D(originV : Point[ThreeD], extentV: Point[ThreeD]) extends BoxedDomain[ThreeD] {
  def dimensionality = 3
  def origin = originV
  def extent = extentV  
}

