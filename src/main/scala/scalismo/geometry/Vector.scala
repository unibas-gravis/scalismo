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
import scalismo.common.Vectorizer
import spire.algebra.Field

import scala.language.implicitConversions

/**
 * An n-dimensional Vector
 */
sealed abstract class Vector[D <: Dim: NDSpace] {
  def apply(i: Int): Double

  def dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def norm: Double = math.sqrt(norm2)

  def norm2: Double

  def *(s: Double): Vector[D]

  def *:(s: Double): Vector[D] = this * s

  def /(s: Double): Vector[D] = this * (1.0 / s)

  def +(that: Vector[D]): Vector[D]

  def -(that: Vector[D]): Vector[D]

  def unary_- : Vector[D] = this * (-1.0)

  def toPoint: Point[D]

  def dot(that: Vector[D]): Double

  def :*(that: Vector[D]): Vector[D]

  def normalize: Vector[D] = this / norm

  def outer(that: Vector[D]): SquareMatrix[D] = {

    require(that.dimensionality == dimensionality)
    val d = dimensionality

    val data = new Array[Double](d * d)
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

  def toArray: Array[Double]

  @deprecated("real data is now private, use toArray", "")
  def data = toArray

  def toBreezeVector = DenseVector(toArray)

  def toFloatBreezeVector = DenseVector(toArray.map(_.toFloat))

  def mapWithIndex(f: (Double, Int) => Double): Vector[D]

  def map(f: Double => Double): Vector[D] = mapWithIndex((v, i) => f(v))

}

/** 1D Vector */
case class Vector1D(x: Double) extends Vector[_1D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException("Vector1D has only 1 element")
  }

  override def +(that: Vector[_1D]): Vector1D = Vector1D(x + that.x)

  override def -(that: Vector[_1D]): Vector1D = Vector1D(x - that.x)

  override def norm2: Double = x * x

  override def dot(that: Vector[_1D]): Double = x * that.x

  override def *(s: Double): Vector1D = Vector1D(x * s)

  override def :*(that: Vector[_1D]): Vector1D = Vector1D(x * that.x)

  override def toPoint: Point1D = Point1D(x)

  override def toArray = Array(x)

  override def mapWithIndex(f: (Double, Int) => Double): Vector1D = Vector1D(f(x, 0))

}

object Vector1D {
  val unit = Vector1D(1.0)
  val zero = Vector1D(0.0)
  val ones = Vector1D(1.0)
}

/** 2D Vector */
case class Vector2D(x: Double, y: Double) extends Vector[_2D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException("Vector2D has only 2 elements")
  }

  override def +(that: Vector[_2D]): Vector2D = Vector2D(x + that.x, y + that.y)

  override def -(that: Vector[_2D]): Vector2D = Vector2D(x - that.x, y - that.y)

  override def norm2: Double = x * x + y * y

  override def dot(that: Vector[_2D]): Double = x * that.x + y * that.y

  override def *(s: Double): Vector2D = Vector2D(x * s, y * s)

  override def :*(that: Vector[_2D]): Vector2D = Vector2D(x * that.x, y * that.y)

  override def toPoint: Point2D = Point2D(x, y)

  override def toArray = Array(x, y)

  override def mapWithIndex(f: (Double, Int) => Double): Vector2D = Vector2D(f(x, 0), f(y, 1))

}

object Vector2D {
  val unitX = Vector2D(1.0, 0.0)
  val unitY = Vector2D(0.0, 1.0)

  val zero = Vector2D(0.0, 0.0)
  val ones = Vector2D(1.0, 1.0)
}

/** 3D Vector */
case class Vector3D(x: Double, y: Double, z: Double) extends Vector[_3D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException("Vector3D has only 3 elements")
  }

  override def +(that: Vector[_3D]): Vector3D = Vector3D(x + that.x, y + that.y, z + that.z)

  override def -(that: Vector[_3D]): Vector3D = Vector3D(x - that.x, y - that.y, z - that.z)

  override def norm2: Double = x * x + y * y + z * z

  override def dot(that: Vector[_3D]): Double = x * that.x + y * that.y + z * that.z

  override def *(s: Double): Vector3D = Vector3D(x * s, y * s, z * s)

  override def :*(that: Vector[_3D]): Vector3D = Vector3D(x * that.x, y * that.y, z * that.z)

  override def toPoint: Point[_3D] = Point3D(x, y, z)

  override def toArray = Array(x, y, z)

  def crossproduct(v: Vector3D): Vector3D = {
    Vector3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  override def mapWithIndex(f: (Double, Int) => Double): Vector3D = Vector3D(f(x, 0), f(y, 1), f(z, 2))

}

object Vector3D {
  val unitX = Vector3D(1.0, 0.0, 0.0)
  val unitY = Vector3D(0.0, 1.0, 0.0)
  val unitZ = Vector3D(0.0, 0.0, 1.0)

  val zero = Vector3D(0.0, 0.0, 0.0)
  val ones = Vector3D(1.0, 1.0, 1.0)
}

object Vector {

  /** creation typeclass */
  trait Create[D <: Dim] {
    def createVector(data: Array[Double]): Vector[D]
    val zero: Vector[D]
  }

  trait Create1D extends Create[_1D] {
    override def createVector(d: Array[Double]) = {
      require(d.length == 1, "Creation of Vector failed: provided Array has invalid length")
      Vector1D(d(0))
    }
    override val zero: Vector[_1D] = Vector1D.zero
  }

  trait Create2D extends Create[_2D] {
    override def createVector(d: Array[Double]) = {
      require(d.length == 2, "Creation of Vector failed: provided Array has invalid length")
      Vector2D(d(0), d(1))
    }
    override val zero: Vector[_2D] = Vector2D.zero
  }

  trait Create3D extends Create[_3D] {
    override def createVector(d: Array[Double]) = {
      require(d.length == 3, "Creation of Vector failed: provided Array has invalid length")
      Vector3D(d(0), d(1), d(2))
    }
    override val zero: Vector[_3D] = Vector3D.zero
  }

  def apply[D <: Dim: NDSpace](d: Array[Double])(implicit builder: Create[D]) = builder.createVector(d)

  def apply(x: Double): Vector[_1D] = Vector1D(x)

  def apply(x: Double, y: Double): Vector[_2D] = Vector2D(x, y)

  def apply(x: Double, y: Double, z: Double): Vector[_3D] = Vector3D(x, y, z)

  def zeros[D <: Dim: NDSpace](implicit builder: Create[D]): Vector[D] = builder.zero

  def fromBreezeVector[D <: Dim: NDSpace](breeze: DenseVector[Double])(implicit builder: Create[D]): Vector[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(breeze.size == dim, s"Invalid size of breeze vector (${breeze.size} != $dim)")
    Vector.apply[D](breeze.data)
  }

  /**
   * create a Cartesian vector from polar coordinates
   *
   * @param r radial distance, 0 .. infinity
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromPolar(r: Double, phi: Double): Vector[_2D] = Vector(
    r * math.cos(phi),
    r * math.sin(phi))

  /**
   * create a Cartesian vector from spherical coordinates
   *
   * @param r radial distance, 0 .. infinity
   * @param theta inclination, 0 .. Pi
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromSpherical(r: Double, theta: Double, phi: Double): Vector[_3D] = Vector(
    (r * math.cos(phi) * math.sin(theta)),
    (r * math.sin(phi) * math.sin(theta)),
    r * math.cos(theta))

  /** spire VectorSpace implementation for Vector */
  implicit def spireVectorSpace[D <: Dim: NDSpace] = new spire.algebra.VectorSpace[Vector[D], Double] {
    override implicit def scalar: Field[Double] = Field[Double]
    override def timesl(r: Double, v: Vector[D]): Vector[D] = v.map(f => f * r)
    override def negate(x: Vector[D]): Vector[D] = x.map(f => -f)
    override def zero: Vector[D] = zeros[D]
    override def plus(x: Vector[D], y: Vector[D]): Vector[D] = x.mapWithIndex((f, i) => f + y(i))
  }

  object implicits {
    implicit def Vector1DToDouble(v: Vector[_1D]): Double = v.x
    implicit def doubleToVector1D(f: Double): Vector[_1D] = Vector(f)
    implicit def tupleOfDoubleToVector2D(t: (Double, Double)): Vector[_2D] = Vector(t._1, t._2)
    implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)): Vector[_3D] = Vector(t._1, t._2, t._3)
  }

  implicit def parametricToConcrete1D(p: Vector[_1D]): Vector1D = p.asInstanceOf[Vector1D]
  implicit def parametricToConcrete2D(p: Vector[_2D]): Vector2D = p.asInstanceOf[Vector2D]
  implicit def parametricToConcrete3D(p: Vector[_3D]): Vector3D = p.asInstanceOf[Vector3D]

  class VectorVectorizer[D <: Dim: NDSpace] extends Vectorizer[Vector[D]] {
    override def dim: Int = implicitly[NDSpace[D]].dimensionality

    override def vectorize(v: Vector[D]): DenseVector[Double] = v.toBreezeVector

    override def unvectorize(d: DenseVector[Double]): Vector[D] = {
      fromBreezeVector(d)
    }

    override def equals(that: Any): Boolean = {
      that match {
        case t: VectorVectorizer[D] => true
        case _ => false
      }
    }
  }

  implicit val Vector1DVectorizer = new VectorVectorizer[_1D]
  implicit val Vector2DVectorizer = new VectorVectorizer[_2D]
  implicit val Vector3DVectorizer = new VectorVectorizer[_3D]

}

