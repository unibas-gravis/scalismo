package org.statismo.stk.core.geometry



/** a marker trait to distinguish the dimension */
sealed trait Dim
trait _1D extends Dim
trait _2D extends Dim
trait _3D extends Dim

trait DimOps[D <: Dim] {
  def toInt : Int
  def matrixNxN : MatrixFactory[D]
  def vector : VectorFactory[D]
  def point : PointFactory[D]
  def index : IndexFactory[D]
}

object Dim {
  implicit object oneDOps extends DimOps[_1D] {
    override def toInt = 1
    override def matrixNxN : MatrixFactory[_1D] = matrixFactory1D
    override def point : PointFactory[_1D] = pointFactory1D
    override def vector : VectorFactory[_1D] = vectorFactory1D
    override def index : IndexFactory[_1D] = indexFactory1D
  }
  implicit object twoDOps extends DimOps[_2D] {
    override def toInt = 2
    override def matrixNxN : MatrixFactory[_2D] = matrixFactory2D
    override def point : PointFactory[_2D] = pointFactory2D
    override def vector : VectorFactory[_2D] = vectorFactory2D
    override def index : IndexFactory[_2D] = indexFactory2D
  }
  implicit object threeDOps extends DimOps[_3D] {
    override def toInt = 3
    override def matrixNxN : MatrixFactory[_3D] = matrixFactory3D
    override def point : PointFactory[_3D] = pointFactory3D
    override def vector : VectorFactory[_3D] = vectorFactory3D
    override def index : IndexFactory[_3D] = indexFactory3D
  }



}
