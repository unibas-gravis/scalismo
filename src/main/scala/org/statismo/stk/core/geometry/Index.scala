package org.statismo.stk.core.geometry

import scala.language.implicitConversions

class Index[D <: Dim : NDSpace] private(val data: Array[Int]) extends Coordinate[D, Int] {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Index[D]]
}


object Index {

  def apply[D <: Dim : NDSpace](d: Array[Int]) = new Index[D](d)
  def apply(i: Int): Index[_1D] = new Index[_1D](Array(i))
  def apply(i: Int, j: Int): Index[_2D] = new Index[_2D](Array(i, j))
  def apply(i: Int, j: Int, k: Int): Index[_3D] = new Index[_3D](Array(i, j, k))

  object implicits {
    implicit def index1DToInt(v: Index[_1D]) = v(0)
    implicit def intToindex1De(i: Int): Index[_1D] = Index(i)
    implicit def tupleOfIntToindex2D(t: (Int, Int)): Index[_2D] = Index(t._1, t._2)
    implicit def tupleOfIntToindex3D(t: (Int, Int, Int)): Index[_3D] = Index(t._1, t._2, t._3)
  }
}


