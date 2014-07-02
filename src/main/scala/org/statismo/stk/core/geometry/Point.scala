package org.statismo.stk.core.geometry


/*======================================
  * Point definitions
  *=======================================*/

/**
  * An ND Point
  */
abstract class Point[D <: Dim : DimTraits : ToInt] extends Coordinate[D, Float] { self: Coordinate[D, Float] =>

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

private case class Point1D(v: Float) extends Point[OneD] with PointLike[OneD, Point1D, Vector1D] {

  def createPoint(data: Array[Float]) = Point1D(data(0))
  def createVector(data: Array[Float]) = Vector1D(data(0))

  override val data = Array(v)
  override def apply(i: Int) = if (i == 0) v else throw new ArrayIndexOutOfBoundsException("index $i > 0")
}

private case class Point2D(x: Float, y: Float) extends Point[TwoD] with PointLike[TwoD, Point2D, Vector2D] {

  def createPoint(data: Array[Float]) = Point2D(data(0), data(1))
  def createVector(data: Array[Float]) = Vector2D(data(0), data(1))

  override val data = Array(x, y)
  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else throw new ArrayIndexOutOfBoundsException("index $i > 1")
  }
}

private case class Point3D(x: Float, y: Float, z: Float) extends Point[ThreeD] with PointLike[ThreeD, Point3D, Vector3D] {

  def createPoint(data: Array[Float]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))

  override val data = Array(x, y, z)
  override def apply(i: Int) = {
    data(i)
    //if (i == 0) x else if (i == 1) y else if (i == 2) z else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

}


object Point {
  trait PointFactory[D <: Dim] { def create(d : Array[Float]) : Point[D] }

  implicit object pointFactory1D extends PointFactory[OneD] {
    override def create(d: Array[Float]) : Point[OneD] = {
      if (d.size != 1)
        throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
      Point1D(d(0))
    }
  }

  implicit object pointFactory2D extends PointFactory[TwoD] {
    override def create(d: Array[Float]) : Point[TwoD] = {
      if (d.size != 2)
        throw new Exception(s"Require array of size 2 to create a Point2D (got ${d.size}")
      Point2D(d(0), d(1))
    }
  }

  implicit object pointFactory3D extends PointFactory[ThreeD] {
    override def create(d: Array[Float]) : Point[ThreeD] = {
      if (d.size != 3)
        throw new Exception(s"Require array of size 3 to create a Point3D (got ${d.size}")
      Point3D(d(0), d(1), d(2))
    }
  }



  def apply(x : Float) : Point[OneD] = new Point1D(x)
  def apply(x : Float, y : Float) : Point[TwoD] = new Point2D(x, y)
  def apply(x : Float, y : Float, z : Float) : Point[ThreeD] = new Point3D(x, y, z)

  def apply[D <: Dim : PointFactory](d : Array[Float]) = implicitly[PointFactory[D]].create(d)




  implicit def Point1DToDouble(p: Point[OneD]) = p(0)
    implicit def doubleToPoint1De(d: Double) : Point[OneD] = Point1D(d.toFloat)
    implicit def tupleOfDoubleToPoint2D(t: (Double, Double)) : Point[TwoD] = Point2D(t._1.toFloat, t._2.toFloat)
    implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)) : Point[ThreeD] = Point3D(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}
