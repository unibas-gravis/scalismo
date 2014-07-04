package org.statismo.stk.core.geometry

import breeze.linalg.DenseVector

/**
 * Created by luethi on 7/1/14.
 */
/**
 * The basic N-tuple in R^N with scalar type S
 */
abstract class Coordinate[D <: Dim : DimOps, @specialized(Int, Float, Double) S] {
  val data: Array[S]
  val dimensionality: Int = implicitly[DimOps[D]].toInt
  def apply(i: Int): S = data(i)
  def toBreezeVector = DenseVector(data)

  override def hashCode = data.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Coordinate[D, S] => {
      that.canEqual(this) && this.data.deep == that.data.deep
    }
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Coordinate[D, S]]

  override def toString() = {
    val s = new StringBuilder("(")
    for (i <- 0 until dimensionality) {
        s ++= data(i).toString
        if (i < dimensionality - 1) s ++= ","
      }
    s ++= ")"
    s.toString
  }

}

