package org.statismo.stk.core.geometry

/**
 * Created by luethi on 7/1/14.
 */
/*======================================
 * Index definitions
 *=======================================*/

abstract class Index[D <: Dim: DimTraits] extends Coordinate[D, Int] {}

case class Index1D(i: Int) extends Index[OneD] {
  val data = Array(i)
  override def apply(id: Int) = {
    if (id == 0) i else throw new ArrayIndexOutOfBoundsException("index $i > 0")
  }

}

case class Index2D(i: Int, j: Int) extends Index[TwoD] {
  val data = Array(i, j)

  override def apply(id: Int) = {
    if (id == 0) i else if (id == 1) j else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}

case class Index3D(i: Int, j: Int, k: Int) extends Index[ThreeD] {
  val data = Array(i, j, k)
  override def apply(id: Int) = {
    if (id == 0) i else if (id == 1) j else if (id == 2) k else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}

object Index {

  implicit def index1DToDouble(v: Index[OneD]) = v(0)
  implicit def intToindex1De(i: Int) = Index1D(i)
  implicit def tupleOfDoubleToindex2D(t: (Int, Int)) = Index2D(t._1, t._2)
  implicit def tupleOfDoubleToindex3D(t: (Int, Int, Int)) = Index3D(t._1, t._2, t._3)
}


