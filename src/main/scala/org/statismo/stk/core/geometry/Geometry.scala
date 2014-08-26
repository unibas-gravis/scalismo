package org.statismo.stk.core.geometry



/** a marker trait to distinguish the dimension */
sealed trait Dim
trait _1D extends Dim
trait _2D extends Dim
trait _3D extends Dim

trait ToInt[D <: Dim] {
  def toInt : Int
}

object Dim {
  implicit object oneDOps extends ToInt[_1D] {
    override def toInt = 1
  }
  implicit object twoDOps extends ToInt[_2D] {
    override def toInt = 2
  }
  implicit object threeDOps extends ToInt[_3D] {
    override def toInt = 3
  }



}
