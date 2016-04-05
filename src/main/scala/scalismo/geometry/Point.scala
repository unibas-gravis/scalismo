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
import scala.reflect.ClassTag

/**
 * An n-dimensional Point
 */
sealed abstract class Point[D <: Dim: NDSpace] {
  def apply(i: Int): Float

  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def +(that: Vector[D]): Point[D]

  def -(that: Vector[D]): Point[D]

  def -(that: Point[D]): Vector[D]

  def toVector: Vector[D]

  def toArray: Array[Float]

  @deprecated("real data is now private, use toArray", "")
  def data = toArray

  def toBreezeVector = DenseVector(toArray)

  def mapWithIndex(f: (Float, Int) => Float): Point[D]

  def map(f: Float => Float): Point[D] = mapWithIndex((v, i) => f(v))
}

/** 1D point */
case class Point1D(x: Float) extends Point[_1D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException("Point1D has only 1 element")
  }

  override def +(that: Vector[_1D]): Point1D = Point1D(x + that.x)

  override def -(that: Vector[_1D]): Point1D = Point1D(x - that.x)

  override def -(that: Point[_1D]): Vector1D = Vector1D(x - that.x)

  override def toVector: Vector1D = Vector1D(x)

  override def toArray = Array(x)

  override def mapWithIndex(f: (Float, Int) => Float): Point1D = Point1D(f(x, 0))
}

object Point1D {
  val origin = Point1D(0f)
}

/** 2D point */
case class Point2D(x: Float, y: Float) extends Point[_2D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException("Point2D has only 2 elements")
  }

  override def +(that: Vector[_2D]): Point2D = Point2D(x + that.x, y + that.y)

  override def -(that: Vector[_2D]): Point2D = Point2D(x - that.x, y - that.y)

  override def -(that: Point[_2D]): Vector2D = Vector2D(x - that.x, y - that.y)

  override def toVector: Vector2D = Vector2D(x, y)

  override def toArray = Array(x, y)

  override def mapWithIndex(f: (Float, Int) => Float): Point2D = Point2D(f(x, 0), f(y, 1))
}

object Point2D {
  val origin = Point2D(0f, 0f)
}

/** 3D point */
case class Point3D(x: Float, y: Float, z: Float) extends Point[_3D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException("Point3D has only 3 elements")
  }

  override def +(that: Vector[_3D]): Point3D = Point3D(x + that.x, y + that.y, z + that.z)

  override def -(that: Vector[_3D]): Point3D = Point3D(x - that.x, y - that.y, z - that.z)

  override def -(that: Point[_3D]): Vector3D = Vector3D(x - that.x, y - that.y, z - that.z)

  override def toVector: Vector[_3D] = Vector3D(x, y, z)

  override def toArray = Array(x, y, z)

  override def mapWithIndex(f: (Float, Int) => Float): Point3D = Point3D(f(x, 0), f(y, 1), f(z, 2))
}

object Point3D {
  val origin = Point3D(0f, 0f, 0f)
}

object Point {

  /** creation typeclass */
  trait Create[D <: Dim] {
    def createPoint(data: Array[Float]): Point[D]
  }

  trait Create1D extends Create[_1D] {
    override def createPoint(d: Array[Float]) = {
      require(d.length == 1)
      Point1D(d(0))
    }
  }

  trait Create2D extends Create[_2D] {
    override def createPoint(d: Array[Float]) = {
      require(d.length == 2)
      Point2D(d(0), d(1))
    }
  }

  trait Create3D extends Create[_3D] {
    override def createPoint(d: Array[Float]) = {
      require(d.length == 3)
      Point3D(d(0), d(1), d(2))
    }
  }

  def apply[D <: Dim: NDSpace](d: Array[Float])(implicit builder: Create[D]) = builder.createPoint(d)
  def apply(x: Float): Point[_1D] = Point1D(x)
  def apply(x: Float, y: Float): Point[_2D] = Point2D(x, y)
  def apply(x: Float, y: Float, z: Float): Point[_3D] = Point3D(x, y, z)

  def origin[D <: Dim: NDSpace](implicit builder: Create[D]) = builder.createPoint(Array.fill(NDSpace[D].dimensionality)(0f))

  def fromBreezeVector[D <: Dim: NDSpace](breeze: DenseVector[Float])(implicit builder: Create[D]): Point[D] = {
    val dim = NDSpace[D].dimensionality
    require(breeze.size == dim, s"Invalid size of breeze vector (${breeze.size} != $dim)")
    Point.apply[D](breeze.data)
  }

  /**
   * create a Cartesian point from polar coordinates
   * @param r radial distance, 0 .. infinity
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromPolar(r: Float, phi: Float): Point[_2D] = Vector.fromPolar(r, phi).toPoint

  /**
   * create a Cartesian point from spherical coordinates
   * @param r radial distance, 0 .. infinity
   * @param theta inclination, 0 .. Pi
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromSpherical(r: Float, theta: Float, phi: Float): Point[_3D] = Vector.fromSpherical(r, theta, phi).toPoint

  object implicits {
    implicit def point1DToFloat(p: Point[_1D]): Float = p.x
    implicit def floatToPoint1D(f: Float): Point[_1D] = Point(f)
    implicit def tupleOfFloatToPoint2D(t: (Float, Float)): Point[_2D] = Point(t._1, t._2)
    implicit def tupleOfFloatToPoint3D(t: (Float, Float, Float)): Point[_3D] = Point(t._1.toFloat, t._2.toFloat, t._3.toFloat)
  }

  implicit def parametricToConcrete1D(p: Point[_1D]): Point1D = p.asInstanceOf[Point1D]
  implicit def parametricToConcrete2D(p: Point[_2D]): Point2D = p.asInstanceOf[Point2D]
  implicit def parametricToConcrete3D(p: Point[_3D]): Point3D = p.asInstanceOf[Point3D]

}
