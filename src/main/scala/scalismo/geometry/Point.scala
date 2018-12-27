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

import scala.language.implicitConversions

/**
 * An n-dimensional Point
 */
sealed abstract class Point[D <: Dim: NDSpace] {
  def apply(i: Int): Double

  def dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  def +(that: SpatialVector[D]): Point[D]

  def -(that: SpatialVector[D]): Point[D]

  def -(that: Point[D]): SpatialVector[D]

  def toVector: SpatialVector[D]

  def toArray: Array[Double]

  @deprecated("real data is now private, use toArray", "")
  def data = toArray

  def toBreezeVector = DenseVector(toArray)

  def mapWithIndex(f: (Double, Int) => Double): Point[D]

  def map(f: Double => Double): Point[D] = mapWithIndex((v, i) => f(v))
}

/** 1D point */
case class Point1D(x: Double) extends Point[_1D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException("Point1D has only 1 element")
  }

  override def +(that: SpatialVector[_1D]): Point1D = Point1D(x + that.x)

  override def -(that: SpatialVector[_1D]): Point1D = Point1D(x - that.x)

  override def -(that: Point[_1D]): SpatialVector1D = SpatialVector1D(x - that.x)

  override def toVector: SpatialVector1D = SpatialVector1D(x)

  override def toArray = Array(x)

  override def mapWithIndex(f: (Double, Int) => Double): Point1D = Point1D(f(x, 0))
}

object Point1D {
  val origin = Point1D(0.0)
  val ones = Point1D(1.0)
}

/** 2D point */
case class Point2D(x: Double, y: Double) extends Point[_2D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException("Point2D has only 2 elements")
  }

  override def +(that: SpatialVector[_2D]): Point2D = Point2D(x + that.x, y + that.y)

  override def -(that: SpatialVector[_2D]): Point2D = Point2D(x - that.x, y - that.y)

  override def -(that: Point[_2D]): SpatialVector2D = SpatialVector2D(x - that.x, y - that.y)

  override def toVector: SpatialVector2D = SpatialVector2D(x, y)

  override def toArray = Array(x, y)

  override def mapWithIndex(f: (Double, Int) => Double): Point2D = Point2D(f(x, 0), f(y, 1))
}

object Point2D {
  val origin = Point2D(0.0, 0.0)
  val ones = Point2D(1.0, 1.0)
}

/** 3D point */
case class Point3D(x: Double, y: Double, z: Double) extends Point[_3D] {
  override def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException("Point3D has only 3 elements")
  }

  override def +(that: SpatialVector[_3D]): Point3D = Point3D(x + that.x, y + that.y, z + that.z)

  override def -(that: SpatialVector[_3D]): Point3D = Point3D(x - that.x, y - that.y, z - that.z)

  override def -(that: Point[_3D]): SpatialVector3D = SpatialVector3D(x - that.x, y - that.y, z - that.z)

  override def toVector: SpatialVector[_3D] = SpatialVector3D(x, y, z)

  override def toArray = Array(x, y, z)

  override def mapWithIndex(f: (Double, Int) => Double): Point3D = Point3D(f(x, 0), f(y, 1), f(z, 2))
}

object Point3D {
  val origin = Point3D(0.0, 0.0, 0.0)
  val ones = Point3D(1.0, 1.0, 1.0)
}

object Point {

  /** creation typeclass */
  trait Create[D <: Dim] {
    def createPoint(data: Array[Double]): Point[D]
  }

  trait Create1D extends Create[_1D] {
    override def createPoint(d: Array[Double]) = {
      require(d.length == 1)
      Point1D(d(0))
    }
  }

  trait Create2D extends Create[_2D] {
    override def createPoint(d: Array[Double]) = {
      require(d.length == 2)
      Point2D(d(0), d(1))
    }
  }

  trait Create3D extends Create[_3D] {
    override def createPoint(d: Array[Double]) = {
      require(d.length == 3)
      Point3D(d(0), d(1), d(2))
    }
  }

  def apply[D <: Dim: NDSpace](d: Array[Double])(implicit builder: Create[D]) = builder.createPoint(d)
  def apply(x: Double): Point[_1D] = Point1D(x)
  def apply(x: Double, y: Double): Point[_2D] = Point2D(x, y)
  def apply(x: Double, y: Double, z: Double): Point[_3D] = Point3D(x, y, z)

  def origin[D <: Dim: NDSpace](implicit builder: Create[D]) = builder.createPoint(Array.fill(NDSpace[D].dimensionality)(0.0))

  def fromBreezeVector[D <: Dim: NDSpace](breeze: DenseVector[Double]): Point[D] = {
    val dim = NDSpace[D].dimensionality
    require(breeze.size == dim, s"Invalid size of breeze vector (${breeze.size} != $dim)")
    Point.apply[D](breeze.data)
  }

  /**
   * create a Cartesian point from polar coordinates
   * @param r radial distance, 0 .. infinity
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromPolar(r: Double, phi: Double): Point[_2D] = SpatialVector.fromPolar(r, phi).toPoint

  /**
   * create a Cartesian point from spherical coordinates
   * @param r radial distance, 0 .. infinity
   * @param theta inclination, 0 .. Pi
   * @param phi azimuth, 0 .. 2*Pi
   */
  def fromSpherical(r: Double, theta: Double, phi: Double): Point[_3D] = SpatialVector.fromSpherical(r, theta, phi).toPoint

  object implicits {
    implicit def point1DToDouble(p: Point[_1D]): Double = p.x
    implicit def DoubleToPoint1D(d: Double): Point[_1D] = Point(d)
    implicit def tupleOfDoubleToPoint2D(t: (Double, Double)): Point[_2D] = Point(t._1, t._2)
    implicit def tupleOfDoubleToPoint3D(t: (Double, Double, Double)): Point[_3D] = Point(t._1, t._2, t._3)
  }

  implicit def parametricToConcrete1D(p: Point[_1D]): Point1D = p.asInstanceOf[Point1D]
  implicit def parametricToConcrete2D(p: Point[_2D]): Point2D = p.asInstanceOf[Point2D]
  implicit def parametricToConcrete3D(p: Point[_3D]): Point3D = p.asInstanceOf[Point3D]

  implicit object Point3DVectorizer extends Vectorizer[Point[_3D]] {
    override def dim: Int = 3
    override def vectorize(pt: Point[_3D]): DenseVector[Double] = pt.toBreezeVector
    override def unvectorize(d: DenseVector[Double]): Point[_3D] = Point(d(0), d(1), d(2))
  }

  implicit object Point2DVectorizer extends Vectorizer[Point[_2D]] {
    override def dim: Int = 2
    override def vectorize(pt: Point[_2D]): DenseVector[Double] = pt.toBreezeVector
    override def unvectorize(d: DenseVector[Double]): Point[_2D] = Point(d(0), d(1))
  }

  implicit object Point1DVectorizer extends Vectorizer[Point[_1D]] {
    override def dim: Int = 1
    override def vectorize(pt: Point[_1D]): DenseVector[Double] = pt.toBreezeVector
    override def unvectorize(d: DenseVector[Double]): Point[_1D] = Point(d(0))
  }

}
