package org.statismo.stk.core.geometry

import breeze.linalg.DenseVector

/**
 * The basic n-tuple in R^n^ with scalar type S
 */
abstract class Coordinate[D <: Dim: NDSpace, @specialized(Int, Float, Double) S] {
  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  private[statismo] val data: Array[S]

  def apply(i: Int): S = data(i)

  def toBreezeVector = DenseVector(data)

  override def hashCode = data.deep.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: Coordinate[D, S] => that.canEqual(this) && this.data.deep == that.data.deep
    case _ => false
  }

  protected def canEqual(other: Any): Boolean

  override def toString = data.deep.toString()
}

