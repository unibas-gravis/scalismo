package org.statismo.stk.core.geometry


/*======================================
  * Point definitions
  *=======================================*/

/**
  * An ND Point
  */
class Point[D <: Dim : DimOps](val data : Array[Float]) extends Coordinate[D, Float] { self: Coordinate[D, Float] =>

  def +(that: Vector[D]): Point[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    implicitly[DimOps[D]].point.create(newData)
  }

  def -(that: Vector[D]): Point[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    implicitly[DimOps[D]].point.create(newData)
  }

  def -(that: Point[D]): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    implicitly[DimOps[D]].vector.create(newData)
  }

  def toVector: Vector[D] = implicitly[DimOps[D]].vector.create(self.data)

  override def hashCode = data.deep.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Point[D] => {
      that.canEqual(this) && this.data.deep == that.data.deep
    }
    case _ => false
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Point[D]]



}


trait PointFactory[D <: Dim] { def create(d : Array[Float]) : Point[D] }

private[geometry] object pointFactory1D extends PointFactory[_1D] {
  override def create(d: Array[Float]) : Point[_1D] = {
    if (d.size != 1)
      throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
    new Point[_1D](d)
  }
}

private[geometry] object pointFactory2D extends PointFactory[_2D] {
  override def create(d: Array[Float]) : Point[_2D] = {
    if (d.size != 2)
      throw new Exception(s"Require array of size 2 to create a Point2D (got ${d.size}")
    new Point[_2D](d)
  }
}

private[geometry] object pointFactory3D extends PointFactory[_3D] {
  override def create(d: Array[Float]) : Point[_3D] = {
    if (d.size != 3)
      throw new Exception(s"Require array of size 3 to create a Point3D (got ${d.size}")
    new Point[_3D](d)
  }
}


object Point {

  def apply(x : Float) : Point[_1D] = new Point[_1D](Array(x))
  def apply(x : Float, y : Float) : Point[_2D] = new Point[_2D](Array(x, y))
  def apply(x : Float, y : Float, z : Float) : Point[_3D] = new Point[_3D](Array(x, y, z))

  def apply[D <: Dim : PointFactory](d : Array[Float]) = implicitly[PointFactory[D]].create(d)



  implicit def Point1DToDouble(p: Point[_1D]) = p(0)
    implicit def doubleToPoint1De(d: Double) : Point[_1D] = Point(d.toFloat)
    implicit def tupleOfDoubleToPoint2D(t: (Double, Double)) : Point[_2D] = Point(t._1.toFloat, t._2.toFloat)
    implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)) : Point[_3D] = Point(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}
