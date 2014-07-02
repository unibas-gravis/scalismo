package org.statismo.stk.core.geometry


/**
 * Created by luethi on 7/1/14.
 */
/*======================================
 * Vector definitions
 *=======================================*/

abstract class Vector[D <: Dim: DimOps] extends Coordinate[D, Float] { self: Vector[D] =>

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


  def createPoint(data: Array[Float]): Point[D]
  def createVector(data: Array[Float]): Vector[D]

  def +(that: Vector[D]): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) + that.data(i)
      i += 1
    }
    createVector(newData)
  }

  def -(that: Vector[D]): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    while (i < dimensionality) {
      newData(i) = this.data(i) - that.data(i)
      i += 1
    }
    createVector(newData)
  }

  def *(s: Double): Vector[D] = {
    var newData = new Array[Float](dimensionality)
    var i = 0
    var sFloat = s.toFloat
    while (i < dimensionality) {
      newData(i) = this.data(i) * sFloat
      i += 1
    }
    createVector(newData)
  }

  def toPoint: Point[D] = createPoint(
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
    implicitly[DimOps[D]].matrixNxN.create(data)
  }


}


private case class Vector1D(x: Float) extends Vector[OneD]  {
  def createPoint(data: Array[Float]) = Point1D(data(0))
  def createVector(data: Array[Float]) = Vector1D(data(0))

  override def apply(i: Int) = {
    if (i == 0) x else throw new ArrayIndexOutOfBoundsException("index $i > 0")
  }

  val data = Array(x)
}

private case class Vector2D(x: Float, y: Float) extends Vector[TwoD] {
  type TVector = Vector2D
  type TPoint = Point2D

  def createPoint(data: Array[Float]) = Point2D(data(0), data(1))
  def createVector(data: Array[Float]) = Vector2D(data(0), data(1))

  override def apply(i: Int) = {
    if (i == 0) x else if (i == 1) y else throw new ArrayIndexOutOfBoundsException("index $i > 1")
  }

  val data = Array(x, y)
}

private case class Vector3D(x: Float, y: Float, z: Float) extends Vector[ThreeD]  {
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

trait VectorFactory[D <: Dim] { def create(d : Array[Float]) : Vector[D] }

private[geometry] object vectorFactory1D extends VectorFactory[OneD] {
  override def create(d: Array[Float]) : Vector[OneD] = {
    if (d.size != 1)
      throw new Exception(s"Require array of size 1 to create a Vector1D (got ${d.size}")
    Vector1D(d(0))
  }
}

private[geometry] object vectorFactory2D extends VectorFactory[TwoD] {
  override def create(d: Array[Float]) : Vector[TwoD] = {
    if (d.size != 2)
      throw new Exception(s"Require array of size 2 to create a Vector2D (got ${d.size}")
    Vector2D(d(0), d(1))
  }
}

private[geometry] object vectorFactory3D extends VectorFactory[ThreeD] {
  override def create(d: Array[Float]) : Vector[ThreeD] = {
    if (d.size != 3)
      throw new Exception(s"Require array of size 3 to create a Vector3D (got ${d.size}")
    Vector3D(d(0), d(1), d(2))
  }
}

object Vector {


  def crossproduct(u : Vector[ThreeD], v : Vector[ThreeD]) : Vector[ThreeD] = {
      Vector3D(u(1) * v(2) - u(2) * v(1), u(2) * v(0) - u(0) * v(2), u(0) * v(1)  - u(1) * v(0))
  }

  def apply(x : Float) : Vector[OneD] = new Vector1D(x)
  def apply(x : Float, y : Float) : Vector[TwoD] = new Vector2D(x, y)
  def apply(x : Float, y : Float, z : Float) : Vector[ThreeD] = new Vector3D(x, y, z)

  def apply[D <: Dim : DimOps](d : Array[Float]) = implicitly[DimOps[D]].vector.create(d)
  def zeros[D <: Dim : DimOps] = {
    val dim = implicitly[DimOps[D]].toInt
    implicitly[DimOps[D]].vector.create(Array.fill[Float](dim)(0f))
  }


  implicit def vector1DToDouble(v: Vector[OneD]) = v(0)
  implicit def doubleToVector1De(d: Double) = Vector(d.toFloat)
  implicit def tupleOfDoubleToVector2D(t: (Double, Double)) = Vector(t._1.toFloat, t._2.toFloat)
  implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)) = Vector(t._1.toFloat, t._2.toFloat, t._3.toFloat)
}


