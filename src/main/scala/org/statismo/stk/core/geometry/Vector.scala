package org.statismo.stk.core.geometry

/**
 * Created by luethi on 7/1/14.
 */
/*======================================
 * Vector definitions
 *=======================================*/

abstract class Vector[D <: Dim: DimTraits] extends Coordinate[D, Float] { self: Vector[D] =>
  val dimTraits = implicitly[DimTraits[D]]

  def +(that: Vector[D]): Vector[D]
  def -(that: Vector[D]): Vector[D]
  def *(s: Double): Vector[D]
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

  def outer(that: Vector[D]): MatrixNxN[D]
  def dot(that: Vector[D]): Float
  def toPoint: Point[D]
}

trait VectorLike[D <: Dim, VectorRepr <: Vector[D], PointRepr <: Point[D]] { self: Vector[D] =>

  def createPoint(data: Array[Float]): PointRepr
  def createVector(data: Array[Float]): VectorRepr

  override def +(that: Vector[D]): VectorRepr = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    createVector(newData)
  }

  override def -(that: Vector[D]): VectorRepr = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    createVector(newData)
  }

  override def *(s: Double): VectorRepr = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    var sFloat = s.toFloat
    while (i < dimensionality) {
      newData(i) = this.data(i) * sFloat
      i += 1
    }
    createVector(newData)
  }

  def toPoint: PointRepr = createPoint(
    self.data)

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
    dimTraits.createMatrixNxN(data)
  }

}

case class Vector1D(x: Float) extends Vector[OneD] with VectorLike[OneD, Vector1D, Point1D] {
  def createPoint(data: Array[Float]) = Point1D(data(0))
  def createVector(data: Array[Float]) = Vector1D(data(0))

  override def apply(i: Int) = {
    if (i == 0) x else throw new ArrayIndexOutOfBoundsException("index $i > 0")
  }

  val data = Array(x)
}

case class Vector2D(x: Float, y: Float) extends Vector[TwoD] with VectorLike[TwoD, Vector2D, Point2D] {
  type TVector = Vector2D
  type TPoint = Point2D

  def createPoint(data: Array[Float]) = Point2D(data(0), data(1))
  def createVector(data: Array[Float]) = Vector2D(data(0), data(1))

  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else throw new ArrayIndexOutOfBoundsException("index $i > 1")
  }

  val data = Array(x, y)
}

case class Vector3D(x: Float, y: Float, z: Float) extends Vector[ThreeD] with VectorLike[ThreeD, Vector3D, Point3D] {
  type TVector = Vector3D
  type TPoint = Point3D

  def createPoint(data: Array[Float]) = Point3D(data(0), data(1), data(2))
  def createVector(data: Array[Float]) = Vector3D(data(0), data(1), data(2))

  def cross(that: Vector3D): Vector3D = {
    Vector3D(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y  - y * that.x)
  }

  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else if (i == 2) z else throw new ArrayIndexOutOfBoundsException("index $i > 2")
  }

  val data = Array(x, y, z)
}


object Vector {
  implicit def vector1DToDouble(v: Vector[OneD]) = v(0)
  implicit def doubleToVector1De(d: Double) = Vector1D(d.toFloat)
  implicit def tupleOfDoubleToVector2D(t: (Double, Double)) = Vector2D(t._1.toFloat, t._2.toFloat)
  implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)) = Vector3D(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}


