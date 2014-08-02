package org.statismo.stk.core.geometry

/**
 * Created by luethi on 7/1/14.
 */
/*======================================
 * Index definitions
 *=======================================*/

class Index[D <: Dim : NDSpaceOps](val data : Array[Int]) extends Coordinate[D, Int] {}

private[geometry] trait IndexFactory[D <: Dim] {
  def create(d : Array[Int]) : Index[D]
}

private[geometry] object indexFactory1D extends IndexFactory[_1D] {
  override def create(d: Array[Int]): Index[_1D] = {
    if (d.size != 1)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Index[_1D](d)
  }
}


private[geometry] object indexFactory2D extends IndexFactory[_2D] {
  override def create(d: Array[Int]): Index[_2D] = {
    if (d.size != 2)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Index[_2D](d)
  }
}


private[geometry] object indexFactory3D extends IndexFactory[_3D] {
  override def create(d: Array[Int]): Index[_3D] = {
    if (d.size != 3)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Index[_3D](d)
  }
}


object Index {

  def apply(i : Int) : Index[_1D] = new Index[_1D](Array(i))
  def apply(i : Int, j : Int) : Index[_2D] = new Index[_2D](Array(i, j))
  def apply(i : Int, j : Int, k : Int) : Index[_3D] = new Index[_3D](Array(i, j, k))

  implicit def index1DToDouble(v: Index[_1D]) = v(0)
  implicit def intToindex1De(i: Int) : Index[_1D] = Index(i)
  implicit def tupleOfDoubleToindex2D(t: (Int, Int)) : Index[_2D] = Index(t._1, t._2)
  implicit def tupleOfDoubleToindex3D(t: (Int, Int, Int)) : Index[_3D] = Index(t._1, t._2, t._3)
}


