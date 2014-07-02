package org.statismo.stk.core.geometry



/** a marker trait to distinguish the dimension */
sealed trait Dim
trait OneD extends Dim
trait TwoD extends Dim
trait ThreeD extends Dim

//trait ToInt[D <: Dim] { def toInt : Int }

trait DimOps[D <: Dim] {
  def toInt : Int
  def matrixNxN : MatrixFactory[D]
  def vector : VectorFactory[D]
  def point : PointFactory[D]
//  def index : IndexFactory[D]
}

//trait DimOps extends ToInt with MatrixNxN.MatrixFactory with Vector.VectorFactory

object Dim {
  implicit object oneDOps extends DimOps[OneD] {
    override def toInt = 1
    override def matrixNxN : MatrixFactory[OneD] = matrixFactory1D
    override def point : PointFactory[OneD] = pointFactory1D
    override def vector : VectorFactory[OneD] = vectorFactory1D
  }
  implicit object twoDOps extends DimOps[TwoD] {
    override def toInt = 2
    override def matrixNxN : MatrixFactory[TwoD] = matrixFactory2D
    override def point : PointFactory[TwoD] = pointFactory2D
    override def vector : VectorFactory[TwoD] = vectorFactory2D
  }
  implicit object threeDOps extends DimOps[ThreeD] {
    override def toInt = 3
    override def matrixNxN : MatrixFactory[ThreeD] = matrixFactory3D
    override def point : PointFactory[ThreeD] = pointFactory3D
    override def vector : VectorFactory[ThreeD] = vectorFactory3D
  }



}
