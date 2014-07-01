package org.statismo.stk.core.geometry

import breeze.linalg.DenseVector

/**
 * Created by luethi on 7/1/14.
 */
/**
 * The basic N-tuple in R^N with scalar type S
 */
abstract class Coordinate[D <: Dim: DimTraits, @specialized(Int, Float, Double) S] {
  val data: Array[S]
  val dimensionality: Int = implicitly[DimTraits[D]].dimensionality
  def apply(i: Int): S = data(i)
  def toBreezeVector = DenseVector(data)
}

