/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.geometry

import breeze.linalg.DenseVector

import scala.language.implicitConversions

/**
 * An n-dimensional Vector
 */
sealed abstract class Vector[D <: Dim: NDSpace] {
  def apply(i: Int): Float

  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def norm: Double = math.sqrt(norm2)

  def norm2: Double

  def *(s: Float): Vector[D]

  def /(s: Float): Vector[D] = this * (1.0f/s)

  def *(s: Double): Vector[D] = this * s.toFloat

  def /(s: Double): Vector[D] = this / s.toFloat

  def +(that: Vector[D]): Vector[D]

  def -(that: Vector[D]): Vector[D]

  def toPoint: Point[D]

  def dot(that: Vector[D]): Float

  def outer(that: Vector[D]): SquareMatrix[D] = {

    require(that.dimensionality == dimensionality)
    val d = dimensionality

    val data = new Array[Float](d * d)
    var i = 0
    var j = 0
    while (i < d) {
      j = 0
      while (j < d) {
        data(j * d + i) = this(i) * that(j)
        j += 1
      }
      i += 1
    }
    SquareMatrix[D](data)
  }

  def toArray: Array[Float]

  @deprecated def data = toArray

  def toBreezeVector = DenseVector(toArray)

  def mapWithIndex(f: (Float, Int) => Float): Vector[D]

}

/** 1D Vector */
case class Vector1D (x: Float) extends Vector[_1D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException("Vector1D has only 1 element")
  }

  override def +(that: Vector[_1D]): Vector[_1D] = Vector(x+that.x)

  override def -(that: Vector[_1D]): Vector[_1D] = Vector(x-that.x)

  override def norm2: Double = x*x

  override def dot(that: Vector[_1D]): Float = x*that.x

  override def *(s: Float): Vector[_1D] = Vector(x*s)

  override def toPoint: Point[_1D] = Point(x)

  override def toArray = Array(x)

  override def mapWithIndex(f: (Float, Int) => Float): Vector[_1D] = Vector(f(x,0))

}

/** 2D Vector */
case class Vector2D (x: Float, y: Float) extends Vector[_2D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException("Vector2D has only 2 elements")
  }

  override def +(that: Vector[_2D]): Vector[_2D] = Vector(x+that.x, y+that.y)

  override def -(that: Vector[_2D]): Vector[_2D] = Vector(x-that.x, y-that.y)

  override def norm2: Double = x*x+y*y

  override def dot(that: Vector[_2D]): Float = x*that.x + y*that.y

  override def *(s: Float): Vector[_2D] = Vector(x*s, y*s)

  override def toPoint: Point[_2D] = Point(x, y)

  override def toArray = Array(x, y)

  override def mapWithIndex(f: (Float, Int) => Float): Vector[_2D] = Vector(f(x,0), f(y,1))

}

/** 3D Vector */
case class Vector3D (x: Float, y: Float, z: Float) extends Vector[_3D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException("Vector3D has only 3 elements")
  }

  override def +(that: Vector[_3D]): Vector[_3D] = Vector(x + that.x, y+that.y, z+that.z)

  override def -(that: Vector[_3D]): Vector[_3D] = Vector(x - that.x, y-that.y, z-that.z)

  override def norm2: Double = x*x + y*y + z*z

  override def dot(that: Vector[_3D]): Float = x*that.x + y*that.y + z*that.z

  override def *(s: Float): Vector[_3D] = Vector(x*s, y*s, z*s)

  override def toPoint: Point[_3D] = Point(x, y, z)

  override def toArray = Array(x, y, z)

  def crossproduct(v: Vector[_3D]): Vector[_3D] = {
    Vector(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  override def mapWithIndex(f: (Float, Int) => Float): Vector[_3D] = Vector(f(x,0), f(y,1), f(z,2))

}

object Vector {

  /** creation typeclass */
  trait Create[D <: Dim] {
    def create(data: Array[Float]): Vector[D]
  }

  implicit val create1D = new Create[_1D] {
    override def create(d: Array[Float]) = {
      require(d.length == 1, "Creation of Vector failed: provided Array has invalid length")
      Vector1D(d(0))
    }
  }

  implicit val create2D = new Create[_2D] {
    override def create(d: Array[Float]) = {
      require(d.length == 2, "Creation of Vector failed: provided Array has invalid length")
      Vector2D(d(0), d(1))
    }
  }

  implicit val create3D = new Create[_3D] {
    override def create(d: Array[Float]) = {
      require(d.length == 3, "Creation of Vector failed: provided Array has invalid length")
      Vector3D(d(0), d(1), d(2))
    }
  }

  def apply[D <: Dim: NDSpace](d: Array[Float])(implicit builder: Create[D]) = builder.create(d)
  def apply(x: Float): Vector[_1D] = Vector1D(x)
  def apply(x: Float, y: Float): Vector[_2D] = Vector2D(x, y)
  def apply(x: Float, y: Float, z: Float): Vector[_3D] = Vector3D(x, y, z)

  def zeros[D <: Dim: NDSpace](implicit builder: Create[D]) = {
    val dim = implicitly[NDSpace[D]].dimensionality
    Vector.apply[D](Array.fill[Float](dim)(0f))
  }
  
  def fromBreezeVector[D <: Dim: NDSpace](breeze: DenseVector[Float])(implicit builder: Create[D]): Vector[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(breeze.size == dim, s"Invalid size of breeze vector (${breeze.size} != $dim)")
    Vector.apply[D](breeze.data)
  }

  object implicits {
    implicit def Vector1DToFloat(v: Vector[_1D]): Float = v.x
    implicit def floatToVector1D(f: Float): Vector[_1D] = Vector(f)
    implicit def tupleOfFloatToVector2D(t: (Float, Float)): Vector[_2D] = Vector(t._1, t._2)
    implicit def tupleOfFloatToVector3D(t: (Float, Float, Float)): Vector[_3D] = Vector(t._1.toFloat, t._2.toFloat, t._3.toFloat)
  }

  implicit def parametricToConcrete1D(p: Vector[_1D]): Vector1D = p.asInstanceOf[Vector1D]
  implicit def parametricToConcrete2D(p: Vector[_2D]): Vector2D = p.asInstanceOf[Vector2D]
  implicit def parametricToConcrete3D(p: Vector[_3D]): Vector3D = p.asInstanceOf[Vector3D]

}

