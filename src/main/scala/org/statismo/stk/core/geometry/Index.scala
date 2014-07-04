package org.statismo.stk.core.geometry

/**
 * Created by luethi on 7/1/14.
 */
/*======================================
 * Index definitions
 *=======================================*/

class Index[D <: Dim : DimOps](val data : Array[Int]) extends Coordinate[D, Int] {}

private[geometry] trait IndexFactory[D <: Dim] {
  def create(d : Array[Int]) : Index[D]
}

private[geometry] object indexFactory1D extends IndexFactory[OneD] {
  override def create(d: Array[Int]): Index[OneD] = {
    if (d.size != 1)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Index[OneD](d)
  }
}


private[geometry] object indexFactory2D extends IndexFactory[TwoD] {
  override def create(d: Array[Int]): Index[TwoD] = {
    if (d.size != 2)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Index[TwoD](d)
  }
}


private[geometry] object indexFactory3D extends IndexFactory[ThreeD] {
  override def create(d: Array[Int]): Index[ThreeD] = {
    if (d.size != 3)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Index[ThreeD](d)
  }
}


object Index {

  def apply(i : Int) : Index[OneD] = new Index[OneD](Array(i))
  def apply(i : Int, j : Int) : Index[TwoD] = new Index[TwoD](Array(i, j))
  def apply(i : Int, j : Int, k : Int) : Index[ThreeD] = new Index[ThreeD](Array(i, j, k))

  implicit def index1DToDouble(v: Index[OneD]) = v(0)
  implicit def intToindex1De(i: Int) : Index[OneD] = Index(i)
  implicit def tupleOfDoubleToindex2D(t: (Int, Int)) : Index[TwoD] = Index(t._1, t._2)
  implicit def tupleOfDoubleToindex3D(t: (Int, Int, Int)) : Index[ThreeD] = Index(t._1, t._2, t._3)
}


