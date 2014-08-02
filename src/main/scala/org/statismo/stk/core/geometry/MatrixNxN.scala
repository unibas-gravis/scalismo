package org.statismo.stk.core.geometry

import breeze.linalg.DenseMatrix


/**
 * Simple matrix class. The data is stored in column major ordering
 */
class MatrixNxN[D <: Dim: NDSpaceOps](val data : Array[Float]) {

  val dimensionality: Int = implicitly[NDSpaceOps[D]].dimensionality

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
    implicitly[NDSpaceOps[D]].matrixNxN.create(newData)
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
    implicitly[NDSpaceOps[D]].vector.create(newVecData)
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
    implicitly[NDSpaceOps[D]].matrixNxN.create(newData)

  }

  def -(that: MatrixNxN[D]): MatrixNxN[D] = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    implicitly[NDSpaceOps[D]].matrixNxN.create(newData)
  }

  def +(that: MatrixNxN[D]): MatrixNxN[D] = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    implicitly[NDSpaceOps[D]].matrixNxN.create(newData)
  }

   def :*(that: MatrixNxN[D]): MatrixNxN[D] = {
    val dim2 = dimensionality * dimensionality
    var newData = new Array[Float](dim2)
    var i = 0
    while (i < dim2) {
      newData(i) = this.data(i) * that.data(i)
      i += 1
    }
    implicitly[NDSpaceOps[D]].matrixNxN.create(newData)
  }

  def t : MatrixNxN[D] = {
    implicitly[NDSpaceOps[D]].matrixNxN.create(this.toBreezeMatrix.t.data)
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


trait MatrixFactory[D <: Dim] {
  def create(d : Array[Float]) : MatrixNxN[D]
  def eye : MatrixNxN[D]
}


private[geometry] object matrixFactory1D extends MatrixFactory[_1D] {
  override def create(d: Array[Float]) : MatrixNxN[_1D] = {
    if (d.size != 1) {
      throw new Exception(s"Require array of size 4 to create a Matrix2x2 (got ${d.size}")
    }
    new MatrixNxN[_1D](d)
  }
  override def eye : MatrixNxN[_1D] = MatrixNxN(Array(1f))
}


private[geometry] object matrixFactory2D extends MatrixFactory[_2D] {
  override def create(d: Array[Float]) : MatrixNxN[_2D] = {
    if (d.size != 4) {
      throw new Exception(s"Require array of size 4 to create a Matrix2x2 (got ${d.size}")
    }
    new MatrixNxN[_2D](d)
  }
  override def eye : MatrixNxN[_2D]= MatrixNxN[_2D](Array(1f, 0f, 0f, 1f))
}

private[geometry] object matrixFactory3D extends MatrixFactory[_3D] {
  override def create(d: Array[Float]) : MatrixNxN[_3D] = {
    if (d.size != 9) {
      throw new Exception(s"Require array of size 9 to create a Matrix3x3 (got ${d.size}")
    }
    new MatrixNxN[_3D](d)
  }
  override def eye : MatrixNxN[_3D] = MatrixNxN[_3D](Array(1f, 0f, 0f, 0f, 1f, 0f, 0f, 0f, 1f))
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

  def apply[D <: Dim : NDSpaceOps](d : Array[Float]) = implicitly[NDSpaceOps[D]].matrixNxN.create(d)

  def eye[D <: Dim :  NDSpaceOps] : MatrixNxN[D] = implicitly[NDSpaceOps[D]].matrixNxN.eye
  def zeros[D <: Dim: NDSpaceOps]: MatrixNxN[D] = MatrixNxN.fill[D](0)
  def ones[D <: Dim:  NDSpaceOps]: MatrixNxN[D] = MatrixNxN.fill[D](1)

  def fill[D <: Dim: NDSpaceOps](elem: => Float): MatrixNxN[D] = {
    val dim = implicitly[NDSpaceOps[D]].dimensionality
    val data = Array.fill[Float](dim * dim)(elem)
    implicitly[NDSpaceOps[D]].matrixNxN.create(data)
  }

  def inv[D <: Dim : NDSpaceOps](m : MatrixNxN[D]) : MatrixNxN[D] = {
    val bm = m.toBreezeMatrix
    val bmInv = breeze.linalg.inv(bm)
    implicitly[NDSpaceOps[D]].matrixNxN.create(bmInv.data.map(_.toFloat))
  }

}
