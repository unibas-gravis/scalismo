package org.statismo.stk.core.geometry

import breeze.linalg.DenseMatrix

/**
 * Created by luethi on 7/1/14.
 */
/////////////////////////////////////
// Matrix
/////////////////////////////////////
/**
 * Simple matrix class. The data is stored in column major ordering
 */
abstract class MatrixNxN[D <: Dim: DimTraits] {
  val dimensionality: Int = implicitly[DimTraits[D]].dimensionality
  def data: Array[Float]
  def apply(i: Int, j: Int): Float = {
    val d = dimensionality
    data(i + d * j)
  }
  def toBreezeMatrix: DenseMatrix[Float] = DenseMatrix.create(dimensionality, dimensionality, data)
  def *(that: Vector[D]): Vector[D]
  def *(that: MatrixNxN[D]): MatrixNxN[D]
  def :*(that: MatrixNxN[D]): MatrixNxN[D]

  def *(f: Float): MatrixNxN[D]
  def *(d: Double): MatrixNxN[D] = this * d.toFloat
  def +(that: MatrixNxN[D]): MatrixNxN[D]
  def -(that: MatrixNxN[D]): MatrixNxN[D]
  def t : MatrixNxN[D]

  override def hashCode = data.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: MatrixNxN[D] => {
      that.canEqual(this) && this.data.deep == that.data.deep
    }
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[MatrixNxN[D]]

  override def toString() = {
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
    s.toString
  }

}

trait MatrixNxNLike[D <: Dim, MatrixRepr <: MatrixNxN[D], VectorRepr <: Vector[D]] extends MatrixNxN[D] { self: MatrixNxN[D] =>

  def createVector(data: Array[Float]): VectorRepr
  def createMatrix(data: Array[Float]): MatrixRepr

  override def *(s: Float): MatrixRepr = {
    val numElements = dimensionality * dimensionality
    val newData = new Array[Float](numElements)
    var i = 0;
    while (i < numElements) {
      newData(i) = data(i) * s
      i += 1
    }
    createMatrix(newData)
  }

  override def *(that: Vector[D]): VectorRepr = {

    var newVecData = new Array[Float](dimensionality)

    var i = 0
    while (i < dimensionality) {
      var v = 0.0f
      var j = 0;
      while (j < dimensionality) {
        v += self(i, j) * that(j)
        j += 1
      }
      newVecData(i) = v
      i += 1
    }
    createVector(newVecData)
  }

  override def *(that: MatrixNxN[D]): MatrixNxN[D] = {

    var newData = new Array[Float](dimensionality * dimensionality)

    var k = 0
    while (k < dimensionality) {
      var i = 0
      while (i < dimensionality) {
        var v = 0.0f
        var j = 0;
        while (j < dimensionality) {
          v += self(i, j) * that(j, k)
          j += 1
        }
        newData(k * dimensionality + i) = v
        i += 1
      }
      k += 1
    }
    createMatrix(newData)

  }

  override def -(that: MatrixNxN[D]): MatrixRepr = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    createMatrix(newData)
  }

  override def +(that: MatrixNxN[D]): MatrixRepr = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    createMatrix(newData)
  }

  override def :*(that: MatrixNxN[D]): MatrixRepr = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) * that.data(i)
      i += 1
    }
    createMatrix(newData)
  }

  override def t : MatrixRepr = {
    createMatrix(this.toBreezeMatrix.t.data)
  }

}

case class Matrix1x1(val data: Array[Float]) extends MatrixNxNLike[OneD, Matrix1x1, Vector1D] {
  override def createMatrix(data: Array[Float]) = Matrix1x1(data)
  override def createVector(data: Array[Float]) = Vector1D(data(0))
}
case class Matrix2x2(val data: Array[Float]) extends MatrixNxNLike[TwoD, Matrix2x2, Vector2D] {
  override def createMatrix(data: Array[Float]) = Matrix2x2(data)
  override def createVector(data: Array[Float]) = Vector2D(data(0), data(1))
}
case class Matrix3x3(val data: Array[Float]) extends MatrixNxNLike[ThreeD, Matrix3x3, Vector3D] {
  override def createMatrix(data: Array[Float]) = Matrix3x3(data)
  override def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))
}

object Matrix1x1 {
  def eye = Matrix1x1((1))
  def zeros = Matrix1x1((0))
  def ones = Matrix1x1((1))
  def apply(f : Float) = new Matrix1x1(Array(f))
}
object Matrix2x2 {
  def eye = Matrix2x2((1, 0), (0, 1))
  def zeros = Matrix2x2((0, 0), (0, 0))
  def ones = Matrix2x2((1, 1), (1, 1))
  def apply(row1: Tuple2[Float, Float], row2: Tuple2[Float, Float]) = {
    new Matrix2x2(Array(row1._1, row2._1, row1._2, row2._2))
  }

}
object Matrix3x3 {
  type TupleF = Tuple3[Float, Float, Float]
  def apply(row1: TupleF, row2: TupleF, row3: TupleF) = {
    new Matrix3x3(Array(row1._1, row2._1, row3._1, row1._2, row2._2, row3._2, row1._3, row2._3, row3._3))
  }
  def eye = Matrix3x3((1, 0, 0), (0, 1, 0), (0, 0, 1))
  def zeros = Matrix3x3((0, 0, 0), (0, 0, 0), (0, 0, 0))
  def ones = Matrix3x3((1, 1, 1), (1, 1, 1), (1, 1, 1))
}

object MatrixNxN {
  def zeros[D <: Dim: DimTraits]: MatrixNxN[D] = MatrixNxN.fill[D](0)

  def ones[D <: Dim: DimTraits]: MatrixNxN[D] = MatrixNxN.fill[D](1)

  def fill[D <: Dim: DimTraits](elem: => Float): MatrixNxN[D] = {
    val dimTraits = implicitly[DimTraits[D]]
    val data = Array.fill[Float](dimTraits.dimensionality * dimTraits.dimensionality)(elem)
    dimTraits.createMatrixNxN(data)
  }

  def inv[D <: Dim : DimTraits](m : MatrixNxN[D]) : MatrixNxN[D] = {
    val dimTraits = implicitly[DimTraits[D]]
    val bm = m.toBreezeMatrix
    val bmInv = breeze.linalg.inv(bm)
    dimTraits.createMatrixNxN(bmInv.data.map(_.toFloat))
  }

}
