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

  @deprecated def data = toArray

  def toBreezeVector: DenseVector[Float] = DenseVector(toArray)

  def mapWithIndex(f: (Float, Int) => Float): Point[D]
}

/** 1D point */
case class Point1D (x: Float) extends Point[_1D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException("Point1D has only 1 element")
  }

  override def +(that: Vector[_1D]): Point1D = Point(x + that.x)

  override def -(that: Vector[_1D]): Point1D = Point(x-that.x)

  override def -(that: Point[_1D]): Vector1D = Vector(x-that.x)

  override def toVector: Vector[_1D] = Vector(x)

  override def toArray = Array(x)

  override def mapWithIndex(f: (Float, Int) => Float): Point[_1D] = Point(f(x,0))
}

/** 2D point */
case class Point2D (x: Float, y: Float) extends Point[_2D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException("Point2D has only 2 elements")
  }

  override def +(that: Vector[_2D]): Point[_2D] = Point(x+that.x, y+that.y)

  override def -(that: Vector[_2D]): Point[_2D] = Point(x-that.x, y-that.y)

  override def -(that: Point[_2D]): Vector[_2D] = Vector(x-that.x, y-that.y)

  override def toVector: Vector[_2D] = Vector(x, y)

  override def toArray = Array(x, y)

  override def mapWithIndex(f: (Float, Int) => Float): Point[_2D] = Point(f(x,0), f(y,1))
}

/** 3D point */
case class Point3D (x: Float, y: Float, z: Float) extends Point[_3D] {
  override def apply(i: Int): Float = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException("Point3D has only 3 elements")
  }

  override def +(that: Vector[_3D]): Point3D = Point(x+that.x, y+that.y, z+that.z)

  override def -(that: Vector[_3D]): Point3D = Point(x-that.x, y-that.y, z-that.z)

  override def -(that: Point[_3D]): Vector3D = Vector(x-that.x, y-that.y, z-that.z)

  override def toVector: Vector[_3D] = Vector(x, y, z)

  override def toArray = Array(x, y, z)

  override def mapWithIndex(f: (Float, Int) => Float): Point[_3D] = Point(f(x,0), f(y,1), f(z,2))
}

object Point {

  /** creation typeclass */
  trait Create[D] {
    def create(data: Array[Float]): Point[D]
  }

  implicit val create1D = new Create[_1D] {
    override def create(d: Array[Float]) = {
      require(d.length == 1)
      Point1D(d(0))
    }
  }

  implicit val create2D = new Create[_2D] {
    override def create(d: Array[Float]) = {
      require(d.length == 2)
      Point2D(d(0), d(1))
    }
  }

  implicit val create3D = new Create[_3D] {
    override def create(d: Array[Float]) = {
      require(d.length == 3)
      Point3D(d(0), d(1), d(2))
    }
  }

  def apply[D <: Dim: NDSpace](d: Array[Float])(implicit builder: Create[D]) = builder.create(d)
  def apply(x: Float): Point[_1D] = Point1D(x)
  def apply(x: Float, y: Float): Point[_2D] = Point2D(x, y)
  def apply(x: Float, y: Float, z: Float): Point[_3D] = Point3D(x, y, z)

  object implicits {
    //implicit def point1DToFloat(p: Point[_1D]): Float = p.x
    implicit def floatToPoint1D(f: Float): Point[_1D] = Point(f)
    implicit def tupleOfFloatToPoint2D(t: (Float, Float)): Point[_2D] = Point(t._1, t._2)
    implicit def tupleOfFloatToPoint3D(t: (Float, Float, Float)): Point[_3D] = Point(t._1.toFloat, t._2.toFloat, t._3.toFloat)
  }

  implicit def parametricToConcrete1D(p: Point[_1D]): Point1D = p.asInstanceOf[Point1D]
  implicit def parametricToConcrete2D(p: Point[_2D]): Point2D = p.asInstanceOf[Point2D]
  implicit def parametricToConcrete3D(p: Point[_3D]): Point3D = p.asInstanceOf[Point3D]

}
