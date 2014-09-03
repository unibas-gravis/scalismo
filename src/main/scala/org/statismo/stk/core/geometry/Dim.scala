package org.statismo.stk.core.geometry

/** a marker trait only meant to distinguish the dimension */
sealed trait Dim

trait _1D extends Dim
trait _2D extends Dim
trait _3D extends Dim

trait DimOps[D <: Dim] {
  def toInt: Int
}

object Dim {
  implicit object OneDOps extends DimOps[_1D] {
    override def toInt = 1
  }
  implicit object TwoDOps extends DimOps[_2D] {
    override def toInt = 2
  }
  implicit object ThreeDOps extends DimOps[_3D] {
    override def toInt = 3
  }
}
