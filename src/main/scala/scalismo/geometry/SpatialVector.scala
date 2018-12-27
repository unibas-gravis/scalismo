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
sealed abstract class SpatialVector[D <: Dim: NDSpace] {
  def apply(i: Int): Double

  def dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def norm: Double = math.sqrt(norm2)

  def norm2: Double

  def *(s: Double): SpatialVector[D]

  def *:(s: Double): SpatialVector[D] = this * s

  def /(s: Double): SpatialVector[D] = this * (1.0 / s)

  def +(that: SpatialVector[D]): SpatialVector[D]

  def -(that: SpatialVector[D]): SpatialVector[D]

  def unary_- : SpatialVector[D] = this * (-1.0)

  def toPoint: Point[D]

  def dot(that: SpatialVector[D]): Double

  def :*(that: SpatialVector[D]): SpatialVector[D]

  def normalize: SpatialVector[D] = this / norm

  def outer(that: SpatialVector[D]): SquareMatrix[D] = {

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

  def mapWithIndex(f: (Double, Int) => Double): SpatialVector[D]

  def map(f: Double => Double): SpatialVector[D] = mapWithIndex((v, i) => f(v))

}

/** 1D Spatial Vector */
case class SpatialVector1D(x: Double) extends SpatialVector[_1D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException("Vector1D has only 1 element")
  }

  override def +(that: SpatialVector[_1D]): SpatialVector1D = SpatialVector1D(x + that.x)

  override def -(that: SpatialVector[_1D]): SpatialVector1D = SpatialVector1D(x - that.x)

  override def norm2: Double = x * x

  override def dot(that: SpatialVector[_1D]): Double = x * that.x

  override def *(s: Double): SpatialVector1D = SpatialVector1D(x * s)

  override def :*(that: SpatialVector[_1D]): SpatialVector1D = SpatialVector1D(x * that.x)

  override def toPoint: Point1D = Point1D(x)

  override def toArray = Array(x)

  override def mapWithIndex(f: (Double, Int) => Double): SpatialVector1D = SpatialVector1D(f(x, 0))

}

object SpatialVector1D {
  val unit = SpatialVector1D(1.0)
  val zero = SpatialVector1D(0.0)
  val ones = SpatialVector1D(1.0)
}

/** 2D Vector */
case class SpatialVector2D(x: Double, y: Double) extends SpatialVector[_2D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException("Vector2D has only 2 elements")
  }

  override def +(that: SpatialVector[_2D]): SpatialVector2D = SpatialVector2D(x + that.x, y + that.y)

  override def -(that: SpatialVector[_2D]): SpatialVector2D = SpatialVector2D(x - that.x, y - that.y)

  override def norm2: Double = x * x + y * y

  override def dot(that: SpatialVector[_2D]): Double = x * that.x + y * that.y

  override def *(s: Double): SpatialVector2D = SpatialVector2D(x * s, y * s)

  override def :*(that: SpatialVector[_2D]): SpatialVector2D = SpatialVector2D(x * that.x, y * that.y)

  override def toPoint: Point2D = Point2D(x, y)

  override def toArray = Array(x, y)

  override def mapWithIndex(f: (Double, Int) => Double): SpatialVector2D = SpatialVector2D(f(x, 0), f(y, 1))

}

object SpatialVector2D {
  val unitX = SpatialVector2D(1.0, 0.0)
  val unitY = SpatialVector2D(0.0, 1.0)

  val zero = SpatialVector2D(0.0, 0.0)
  val ones = SpatialVector2D(1.0, 1.0)
}

/** 3D Vector */
case class SpatialVector3D(x: Double, y: Double, z: Double) extends SpatialVector[_3D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException("Vector3D has only 3 elements")
  }

  override def +(that: SpatialVector[_3D]): SpatialVector3D = SpatialVector3D(x + that.x, y + that.y, z + that.z)

  override def -(that: SpatialVector[_3D]): SpatialVector3D = SpatialVector3D(x - that.x, y - that.y, z - that.z)

  override def norm2: Double = x * x + y * y + z * z

  override def dot(that: SpatialVector[_3D]): Double = x * that.x + y * that.y + z * that.z

  override def *(s: Double): SpatialVector3D = SpatialVector3D(x * s, y * s, z * s)

  override def :*(that: SpatialVector[_3D]): SpatialVector3D = SpatialVector3D(x * that.x, y * that.y, z * that.z)

  override def toPoint: Point[_3D] = Point3D(x, y, z)

  override def toArray = Array(x, y, z)

  def crossproduct(v: SpatialVector3D): SpatialVector3D = {
    SpatialVector3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  override def mapWithIndex(f: (Double, Int) => Double): SpatialVector3D = SpatialVector3D(f(x, 0), f(y, 1), f(z, 2))

}

object SpatialVector3D {
  val unitX = SpatialVector3D(1.0, 0.0, 0.0)
  val unitY = SpatialVector3D(0.0, 1.0, 0.0)
  val unitZ = SpatialVector3D(0.0, 0.0, 1.0)

  val zero = SpatialVector3D(0.0, 0.0, 0.0)
  val ones = SpatialVector3D(1.0, 1.0, 1.0)
}

object SpatialVector {

  /** creation typeclass */
  trait Create[D <: Dim] {
    def createVector(data: Array[Double]): SpatialVector[D]
    val zero: SpatialVector[D]
  }

  trait Create1D extends Create[_1D] {
    override def createVector(d: Array[Double]) = {
      require(d.length == 1, "Creation of Vector failed: provided Array has invalid length")
      SpatialVector1D(d(0))
    }
    override val zero: SpatialVector[_1D] = SpatialVector1D.zero
  }

  trait Create2D extends Create[_2D] {
    override def createVector(d: Array[Double]) = {
      require(d.length == 2, "Creation of Vector failed: provided Array has invalid length")
      SpatialVector2D(d(0), d(1))
    }
    override val zero: SpatialVector[_2D] = SpatialVector2D.zero
  }

  trait Create3D extends Create[_3D] {
    override def createVector(d: Array[Double]) = {
      require(d.length == 3, "Creation of Vector failed: provided Array has invalid length")
      SpatialVector3D(d(0), d(1), d(2))
    }
    override val zero: SpatialVector[_3D] = SpatialVector3D.zero
  }

  def apply[D <: Dim: NDSpace](d: Array[Double])(implicit builder: Create[D]) = builder.createVector(d)

  def apply(x: Double): SpatialVector[_1D] = SpatialVector1D(x)

  def apply(x: Double, y: Double): SpatialVector[_2D] = SpatialVector2D(x, y)

  def apply(x: Double, y: Double, z: Double): SpatialVector[_3D] = SpatialVector3D(x, y, z)

  def zeros[D <: Dim: NDSpace](implicit builder: Create[D]): SpatialVector[D] = builder.zero

  def fromBreezeVector[D <: Dim: NDSpace](breeze: DenseVector[Double]): SpatialVector[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(breeze.size == dim, s"Invalid size of breeze vector (${breeze.size} != $dim)")
    SpatialVector.apply[D](breeze.data)
  }

  /**
   * create a Cartesian vector from polar coordinates
   *
   * @param r radial distance, 0 .. infinity
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromPolar(r: Double, phi: Double): SpatialVector[_2D] = SpatialVector(
    r * math.cos(phi),
    r * math.sin(phi))

  /**
   * create a Cartesian vector from spherical coordinates
   *
   * @param r radial distance, 0 .. infinity
   * @param theta inclination, 0 .. Pi
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromSpherical(r: Double, theta: Double, phi: Double): SpatialVector[_3D] = SpatialVector(
    (r * math.cos(phi) * math.sin(theta)),
    (r * math.sin(phi) * math.sin(theta)),
    r * math.cos(theta))

  /** spire VectorSpace implementation for Vector */
  implicit def spireVectorSpace[D <: Dim: NDSpace] = new spire.algebra.VectorSpace[SpatialVector[D], Double] {
    override implicit def scalar: Field[Double] = Field[Double]
    override def timesl(r: Double, v: SpatialVector[D]): SpatialVector[D] = v.map(f => f * r)
    override def negate(x: SpatialVector[D]): SpatialVector[D] = x.map(f => -f)
    override def zero: SpatialVector[D] = zeros[D]
    override def plus(x: SpatialVector[D], y: SpatialVector[D]): SpatialVector[D] = x.mapWithIndex((f, i) => f + y(i))
  }

  object implicits {
    implicit def Vector1DToDouble(v: SpatialVector[_1D]): Double = v.x
    implicit def doubleToVector1D(f: Double): SpatialVector[_1D] = SpatialVector(f)
    implicit def tupleOfDoubleToVector2D(t: (Double, Double)): SpatialVector[_2D] = SpatialVector(t._1, t._2)
    implicit def tupleOfDoubleToVector3D(t: (Double, Double, Double)): SpatialVector[_3D] = SpatialVector(t._1, t._2, t._3)
  }

  implicit def parametricToConcrete1D(p: SpatialVector[_1D]): SpatialVector1D = p.asInstanceOf[SpatialVector1D]
  implicit def parametricToConcrete2D(p: SpatialVector[_2D]): SpatialVector2D = p.asInstanceOf[SpatialVector2D]
  implicit def parametricToConcrete3D(p: SpatialVector[_3D]): SpatialVector3D = p.asInstanceOf[SpatialVector3D]

  class VectorVectorizer[D <: Dim: NDSpace] extends Vectorizer[SpatialVector[D]] {
    override def dim: Int = implicitly[NDSpace[D]].dimensionality

    override def vectorize(v: SpatialVector[D]): DenseVector[Double] = v.toBreezeVector

    override def unvectorize(d: DenseVector[Double]): SpatialVector[D] = {
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

