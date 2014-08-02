package org.statismo.stk.core.geometry



/** a marker trait to distinguish the dimension */
sealed trait Dim
trait _1D extends Dim
trait _2D extends Dim
trait _3D extends Dim

trait NDSpaceOps[D <: Dim] {
  def dimensionality : Int
  def matrixNxN : MatrixFactory[D]
  def vector : VectorFactory[D]
  def point : PointFactory[D]
  def index : IndexFactory[D]
}

object Dim {
  implicit object oneDOps extends NDSpaceOps[_1D] {
    override def dimensionality = 1
    override def matrixNxN : MatrixFactory[_1D] = matrixFactory1D
    override def point : PointFactory[_1D] = pointFactory1D
    override def vector : VectorFactory[_1D] = vectorFactory1D
    override def index : IndexFactory[_1D] = indexFactory1D
  }
  implicit object twoDOps extends NDSpaceOps[_2D] {
    override def dimensionality = 2
    override def matrixNxN : MatrixFactory[_2D] = matrixFactory2D
    override def point : PointFactory[_2D] = pointFactory2D
    override def vector : VectorFactory[_2D] = vectorFactory2D
    override def index : IndexFactory[_2D] = indexFactory2D
  }
  implicit object threeDOps extends NDSpaceOps[_3D] {
    override def dimensionality = 3
    override def matrixNxN : MatrixFactory[_3D] = matrixFactory3D
    override def point : PointFactory[_3D] = pointFactory3D
    override def vector : VectorFactory[_3D] = vectorFactory3D
    override def index : IndexFactory[_3D] = indexFactory3D
  }



}
