package scalismo.geometry

import scala.language.implicitConversions

/**
 * An n-dimensional Point
 */
class Point[D <: Dim : NDSpace] private(private[scalismo] override val data: Array[Float]) extends Coordinate[D, Float] {

  def +(that: Vector[D]): Point[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    Point[D](newData)
  }

  def -(that: Vector[D]): Point[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Point[D](newData)
  }

  def -(that: Point[D]): Vector[D] = {
    val newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    Vector[D](newData)
  }

  def toVector: Vector[D] = Vector[D](data)

  protected override def canEqual(other: Any): Boolean = other.isInstanceOf[Point[D]]
}


object Point {

  def apply[D <: Dim : NDSpace](d: Array[Float]) = new Point[D](d)
  def apply(x: Float): Point[_1D] = new Point[_1D](Array(x))
  def apply(x: Float, y: Float): Point[_2D] = new Point[_2D](Array(x, y))
  def apply(x: Float, y: Float, z: Float): Point[_3D] = new Point[_3D](Array(x, y, z))

  object implicits {
    implicit def point1DToFloat(p: Point[_1D]) = p(0)
    implicit def floatToPoint1D(f: Float): Point[_1D] = Point(f)
    implicit def tupleOfFloatToPoint2D(t: (Float, Float)): Point[_2D] = Point(t._1, t._2)
    implicit def tupleOfFloatToPoint3D(t: (Float, Float, Float)): Point[_3D] = Point(t._1.toFloat, t._2.toFloat, t._3.toFloat)
  }
}
