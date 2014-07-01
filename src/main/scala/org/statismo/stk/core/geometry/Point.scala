package org.statismo.stk.core.geometry


/*======================================
  * Point definitions
  *=======================================*/

/**
  * An ND Point
  */
abstract class Point[D <: Dim: DimTraits] extends Coordinate[D, Float] { self: Coordinate[D, Float] =>

  def +(that: Vector[D]): Point[D]
  def -(that: Vector[D]): Point[D]
  def -(that: Point[D]): Vector[D]
  def toVector: Vector[D]

}

trait PointLike[D <: Dim, PointRepr <: Point[D], VectorRepr <: Vector[D]] { self: Point[D] =>

  def createPoint(data: Array[Float]): PointRepr
  def createVector(data: Array[Float]): VectorRepr

  override def +(that: Vector[D]): PointRepr = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    createPoint(newData)
  }

  override def -(that: Vector[D]): PointRepr = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    createPoint(newData)
  }

  override def -(that: Point[D]): VectorRepr = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    createVector(newData)
  }

  override def toVector: VectorRepr = createVector(self.data)

}

// Concrete instances for 1D, 2D and 3D
case class Point1D(v: Float) extends Point[OneD] with PointLike[OneD, Point1D, Vector1D] {

  def createPoint(data: Array[Float]) = Point1D(data(0))
  def createVector(data: Array[Float]) = Vector1D(data(0))

  override val data = Array(v)
  override def apply(i: Int) = if (i == 0) v else throw new ArrayIndexOutOfBoundsException("index $i > 0")
}

case class Point2D(x: Float, y: Float) extends Point[TwoD] with PointLike[TwoD, Point2D, Vector2D] {

  def createPoint(data: Array[Float]) = Point2D(data(0), data(1))
  def createVector(data: Array[Float]) = Vector2D(data(0), data(1))

  override val data = Array(x, y)
  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else throw new ArrayIndexOutOfBoundsException("index $i > 1")
  }
}

case class Point3D(x: Float, y: Float, z: Float) extends Point[ThreeD] with PointLike[ThreeD, Point3D, Vector3D] {

  def createPoint(data: Array[Float]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))

  override val data = Array(x, y, z)
  override def apply(i: Int) = {
    data(i)
    //if (i == 0) x else if (i == 1) y else if (i == 2) z else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}


object Point {

    implicit def Point1DToDouble(p: Point[OneD]) = p(0)
    implicit def doubleToPoint1De(d: Double) = Point1D(d.toFloat)
    implicit def tupleOfDoubleToPoint2D(t: (Double, Double)) = Point2D(t._1.toFloat, t._2.toFloat)
    implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)) = Point3D(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}
