package org.statismo.stk.core.geometry


/**
 * Created by luethi on 7/1/14.
 */
/*======================================
 * Vector definitions
 *=======================================*/

class Vector[D <: Dim: ToInt](val data : Array[Float]) extends Coordinate[D, Float] { self: Vector[D] =>

  def norm2: Double = {
    var norm2 = 0.0
    var i = 0;
    while (i < dimensionality) {
      norm2 += data(i) * data(i)
      i += 1
    }
    norm2
  }
  def norm = math.sqrt(norm2)


  def +(that: Vector[D]): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def -(that: Vector[D]): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def *(s: Double): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    var sFloat = s.toFloat
    while (i < dimensionality) {
      newData(i) = this.data(i) * sFloat
      i += 1
    }
    Vector[D](newData)
  }

  def toPoint: Point[D] = new Point[D](self.data)

  def dot(that: Vector[D]): Float = {
    val d = dimensionality

    var dotprod = 0f
    var i = 0;
    while (i < d) {
      dotprod += self(i) * that(i)
      i += 1
    }
    dotprod
  }

  def outer(that: Vector[D]): MatrixNxN[D] = {

    require(that.dimensionality == dimensionality)
    val d = self.dimensionality

    val data = new Array[Float](d * d)
    var i = 0;
    var j = 0;
    while (i < d) {
      j = 0
      while (j < d) {
        data(j * d + i) = self(i) * that(j)
        j += 1
      }
      i += 1
    }
    MatrixNxN[D](data)
  }

  override def hashCode = data.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Vector[D] => {
      that.canEqual(this) && this.data.deep == that.data.deep
    }
    case _ => false
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Vector[D]]

}


object Vector {


  def crossproduct(u : Vector[_3D], v : Vector[_3D]) : Vector[_3D] = {
      Vector(u(1) * v(2) - u(2) * v(1), u(2) * v(0) - u(0) * v(2), u(0) * v(1)  - u(1) * v(0))
  }

  def apply(x : Float) : Vector[_1D] = new Vector[_1D](Array(x))
  def apply(x : Float, y : Float) : Vector[_2D] = new Vector[_2D](Array(x, y))
  def apply(x : Float, y : Float, z : Float) : Vector[_3D] = new Vector[_3D](Array(x, y, z))

  def apply[D <: Dim : ToInt](d : Array[Float]) = new Vector[D](d)
  def zeros[D <: Dim : ToInt] = {
    val dim = implicitly[ToInt[D]].toInt
    new Vector[D](Array.fill[Float](dim)(0f))
  }


  implicit def vector1DToDouble(v: Vector[_1D]) = v(0)
  implicit def doubleToVector1De(d: Double) = Vector(d.toFloat)
  implicit def tupleOfDoubleToVector2D(t: (Double, Double)) = Vector(t._1.toFloat, t._2.toFloat)
  implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)) = Vector(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}


