package org.statismo.stk.core.geometry


/*======================================
  * Point definitions
  *=======================================*/

/**
  * An ND Point
  */
class Point[D <: Dim : ToInt](val data : Array[Float]) extends Coordinate[D, Float] { self: Coordinate[D, Float] =>

  def +(that: Vector[D]): Point[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    Point[D](newData)
  }

  def -(that: Vector[D]): Point[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Point[D](newData)
  }

  def -(that: Point[D]): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def toVector: Vector[D] = Vector[D](self.data)

  override def hashCode = data.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Point[D] => {
      that.canEqual(this) && this.data.deep == that.data.deep
    }
    case _ => false
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Point[D]]



}


object Point {

  def apply(x : Float) : Point[_1D] = new Point[_1D](Array(x))
  def apply(x : Float, y : Float) : Point[_2D] = new Point[_2D](Array(x, y))
  def apply(x : Float, y : Float, z : Float) : Point[_3D] = new Point[_3D](Array(x, y, z))

  def apply[D <: Dim : ToInt](d : Array[Float]) = new Point[D](d)


  implicit def Point1DToDouble(p: Point[_1D]) = p(0)
    implicit def doubleToPoint1De(d: Double) : Point[_1D] = Point(d.toFloat)
    implicit def tupleOfDoubleToPoint2D(t: (Double, Double)) : Point[_2D] = Point(t._1.toFloat, t._2.toFloat)
    implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)) : Point[_3D] = Point(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}
