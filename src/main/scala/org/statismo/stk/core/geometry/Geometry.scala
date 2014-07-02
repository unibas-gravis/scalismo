package org.statismo.stk.core.geometry



/** a marker trait to distinguish the dimension */
sealed trait Dim
trait OneD extends Dim
trait TwoD extends Dim
trait ThreeD extends Dim

trait ToInt[D <: Dim] { def toInt : Int }

//trait DimOps extends ToInt with MatrixNxN.MatrixFactory with Vector.VectorFactory

object Dim {
  implicit object oneDToInt extends ToInt[OneD] { override def toInt = 1}
  implicit object twoDToInt extends ToInt[TwoD] { override def toInt = 2 }
  implicit object threeDToInt extends ToInt[ThreeD] { override def toInt = 3 }



}
