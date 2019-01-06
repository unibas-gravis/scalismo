/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.geometry

import breeze.linalg.DenseMatrix

/**
 * Simple square matrix class of dimension D x D. The data is stored in column major ordering
 */
class SquareMatrix[D: NDSpace] private (private[scalismo] val data: Array[Double]) {

  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality
  val ndSpace = NDSpace[D]

  def apply(i: Int, j: Int): Double = {
    val d = dimensionality
    data(i + d * j)
  }

  def toBreezeMatrix: DenseMatrix[Double] = DenseMatrix.create(dimensionality, dimensionality, data)

  def *(s: Double): SquareMatrix[D] = {
    val numElements = dimensionality * dimensionality
    val newData = new Array[Double](numElements)
    var i = 0
    while (i < numElements) {
      newData(i) = data(i) * s
      i += 1
    }
    SquareMatrix[D](newData)
  }

  def *(that: Vector[D]): Vector[D] = {

    val newData = new Array[Double](dimensionality)

    var i = 0
    while (i < dimensionality) {
      var v = 0.0
      var j = 0
      while (j < dimensionality) {
        v += this(i, j) * that(j)
        j += 1
      }
      newData(i) = v
      i += 1
    }
    Vector[D](newData)
  }

  def *(that: SquareMatrix[D]): SquareMatrix[D] = {

    val newData = new Array[Double](dimensionality * dimensionality)

    var k = 0
    while (k < dimensionality) {
      var i = 0
      while (i < dimensionality) {
        var v = 0.0
        var j = 0
        while (j < dimensionality) {
          v += this(i, j) * that(j, k)
          j += 1
        }
        newData(k * dimensionality + i) = v
        i += 1
      }
      k += 1
    }
    SquareMatrix[D](newData)
  }

  def -(that: SquareMatrix[D]): SquareMatrix[D] = {
    val dim2 = dimensionality * dimensionality
    val newData = new Array[Double](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    SquareMatrix[D](newData)
  }

  def +(that: SquareMatrix[D]): SquareMatrix[D] = {
    val dim2 = dimensionality * dimensionality
    val newData = new Array[Double](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    SquareMatrix[D](newData)
  }

  def :*(that: SquareMatrix[D]): SquareMatrix[D] = {
    val dim2 = dimensionality * dimensionality
    val newData = new Array[Double](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) * that.data(i)
      i += 1
    }
    SquareMatrix[D](newData)
  }

  def t: SquareMatrix[D] = {
    SquareMatrix[D](this.toBreezeMatrix.t.toArray)
  }

  override def hashCode = data.deep.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: SquareMatrix[D] =>
      that.canEqual(this) && this.data.deep == that.data.deep
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SquareMatrix[D]]

  override def toString = {
    val s = new StringBuilder("[")
    for (i <- 0 until dimensionality) {
      s ++= "["
      for (j <- 0 until dimensionality) {
        s ++= this.apply(i, j).toString
        if (j < dimensionality - 1) s ++= ","
      }
      s ++= "]"
      if (i < dimensionality - 1) s ++= "\n"
    }
    s ++= "]"
    s.toString()
  }
}

object SquareMatrix {

  def apply(d: Double): SquareMatrix[_1D] = new SquareMatrix[_1D](Array(d))
  def apply(row1: (Double, Double), row2: (Double, Double)): SquareMatrix[_2D] = {
    new SquareMatrix[_2D](Array(row1._1, row2._1, row1._2, row2._2))
  }

  type TupleF = (Double, Double, Double)
  def apply(row1: TupleF, row2: TupleF, row3: TupleF): SquareMatrix[_3D] = {
    new SquareMatrix[_3D](Array(row1._1, row2._1, row3._1, row1._2, row2._2, row3._2, row1._3, row2._3, row3._3))
  }

  def apply[D: NDSpace](d: Array[Double]) = new SquareMatrix[D](d)

  def eye[D](implicit ev: NDSpace[D]): SquareMatrix[D] = {
    val dim = ev.dimensionality
    val data = Array.fill(dim * dim)(0.0)
    for (i <- 0 until dim) {
      data(i * dim + i) = 1
    }
    new SquareMatrix[D](data)
  }

  def diag[D](vector: Vector[D])(implicit ev: NDSpace[D]): SquareMatrix[D] = {
    val dim = ev.dimensionality
    val data = Array.fill(dim * dim)(0.0)
    for (i <- 0 until dim) {
      data(i * dim + i) = vector(i)
    }
    new SquareMatrix[D](data)
  }

  def zeros[D: NDSpace]: SquareMatrix[D] = SquareMatrix.fill[D](0)
  def ones[D: NDSpace]: SquareMatrix[D] = SquareMatrix.fill[D](1)

  def fill[D: NDSpace](elem: => Double): SquareMatrix[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    val data = Array.fill[Double](dim * dim)(elem)
    new SquareMatrix[D](data)
  }

  def inv[D: NDSpace](m: SquareMatrix[D]): SquareMatrix[D] = {
    val bm = m.toBreezeMatrix
    val bmInv: DenseMatrix[Double] = breeze.linalg.inv(bm)
    new SquareMatrix[D](bmInv.data)
  }
}
