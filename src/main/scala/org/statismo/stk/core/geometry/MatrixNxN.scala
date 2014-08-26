package org.statismo.stk.core.geometry

import breeze.linalg.DenseMatrix


/**
 * Simple matrix class. The data is stored in column major ordering
 */
class MatrixNxN[D <: Dim: DimOps] private (val data : Array[Float]) {

  val dimensionality: Int = implicitly[DimOps[D]].toInt

  def apply(i: Int, j: Int): Float = {
    val d = dimensionality
    data(i + d * j)
  }
  def toBreezeMatrix: DenseMatrix[Float] = DenseMatrix.create(dimensionality, dimensionality, data)

  def *(s: Float): MatrixNxN[D] = {
    val numElements = dimensionality * dimensionality
    val newData = new Array[Float](numElements)
    var i = 0;
    while (i < numElements) {
      newData(i) = data(i) * s
      i += 1
    }
    MatrixNxN[D](newData)
  }

  def *(s : Double) : MatrixNxN[D] = this * s.toFloat

  def *(that: Vector[D]): Vector[D] = {

    var newVecData = new Array[Float](dimensionality)

    var i = 0
    while (i < dimensionality) {
      var v = 0.0f
      var j = 0;
      while (j < dimensionality) {
        v += this(i, j) * that(j)
        j += 1
      }
      newVecData(i) = v
      i += 1
    }
    Vector[D](newVecData)
  }

  def *(that: MatrixNxN[D]): MatrixNxN[D] = {

    var newData = new Array[Float](dimensionality * dimensionality)

    var k = 0
    while (k < dimensionality) {
      var i = 0
      while (i < dimensionality) {
        var v = 0.0f
        var j = 0;
        while (j < dimensionality) {
          v += this(i, j) * that(j, k)
          j += 1
        }
        newData(k * dimensionality + i) = v
        i += 1
      }
      k += 1
    }
    MatrixNxN[D](newData)

  }

  def -(that: MatrixNxN[D]): MatrixNxN[D] = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    MatrixNxN[D](newData)
  }

  def +(that: MatrixNxN[D]): MatrixNxN[D] = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    MatrixNxN[D](newData)
  }

   def :*(that: MatrixNxN[D]): MatrixNxN[D] = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) * that.data(i)
      i += 1
    }
     MatrixNxN[D](newData)
  }

  def t : MatrixNxN[D] = {
    MatrixNxN[D](this.toBreezeMatrix.t.data)
  }


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



object MatrixNxN {


  def apply(f : Float) : MatrixNxN[_1D] = new MatrixNxN[_1D](Array(f))
  def apply(row1: Tuple2[Float, Float], row2: Tuple2[Float, Float]) : MatrixNxN[_2D]= {
    new MatrixNxN[_2D](Array(row1._1, row2._1, row1._2, row2._2))
  }

  type TupleF = Tuple3[Float, Float, Float]
  def apply(row1: TupleF, row2: TupleF, row3: TupleF) : MatrixNxN[_3D] = {
    new MatrixNxN[_3D](Array(row1._1, row2._1, row3._1, row1._2, row2._2, row3._2, row1._3, row2._3, row3._3))
  }

  def apply[D <: Dim : DimOps](d : Array[Float]) = new MatrixNxN[D](d)

  def eye[D <: Dim](implicit ev : DimOps[D]) : MatrixNxN[D] = {
    val dim =  ev.toInt
    val data = Array.fill(dim * dim)(0f)
    for (i <- 0 until dim) {
      data(i * dim + i) = 1
    }
    new MatrixNxN[D](data)

  }
  def zeros[D <: Dim: DimOps]: MatrixNxN[D] = MatrixNxN.fill[D](0)
  def ones[D <: Dim:  DimOps]: MatrixNxN[D] = MatrixNxN.fill[D](1)

  def fill[D <: Dim: DimOps](elem: => Float): MatrixNxN[D] = {
    val dim = implicitly[DimOps[D]].toInt
    val data = Array.fill[Float](dim * dim)(elem)
    new MatrixNxN[D](data)
  }

  def inv[D <: Dim : DimOps](m : MatrixNxN[D]) : MatrixNxN[D] = {
    val bm = m.toBreezeMatrix
    val bmInv = breeze.linalg.inv(bm)
    new MatrixNxN[D](bmInv.data.map(_.toFloat))
  }

}
