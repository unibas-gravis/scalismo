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
package scalismo.numerics

import scalismo.common.Scalar
import scalismo.geometry.SpatialVector._
import scalismo.geometry._
import spire.math.{ UByte, UInt, UShort }

/** defines (convex) interpolation between two values */
trait ValueInterpolator[@specialized(Double, Float) A] {
  def blend(obj1: A, obj2: A, l: Double): A

  /** mix multiple values, forms convex combination (with normalized weights) */
  // loop: better performance than list eating tailrec
  def convexCombination(first: (A, Double), rest: (A, Double)*): A = {
    var mix: A = first._1
    var f: Double = first._2

    for (next <- rest) {
      val s = f + next._2 // f+f1
      if (s > 0f)
        mix = blend(mix, next._1, f / s)
      f = s
    }
    mix
  }

  /** fast explicit barycentric interpolation, most used case of multiple blends */
  def barycentricInterpolation(v1: A, f1: Double, v2: A, f2: Double, v3: A, f3: Double): A = {
    val f12 = f1 + f2
    if (f12 > 0)
      blend(blend(v1, v2, f1 / f12), v3, f12 / (f3 + f12))
    else
      v3
  }

  /** direct access to averaging function, warning: unstable for large sequences! (implement hierarchical blending for better stability) */
  def average(first: A, rest: A*): A = {
    var mix: A = first
    var n = 1
    for (next <- rest) {
      mix = blend(mix, next, n.toDouble / (n + 1))
      n += 1
    }
    mix
  }
}

object ValueInterpolator {

  def apply[A](implicit ev: ValueInterpolator[A]): ValueInterpolator[A] = ev

  implicit val floatInterpolator = new ValueInterpolator[Float] {
    override def blend(obj1: Float, obj2: Float, l: Double): Float = (obj1 * l + obj2 * (1.0 - l)).toFloat
  }

  implicit val doubleInterpolator = new ValueInterpolator[Double] {
    override def blend(obj1: Double, obj2: Double, l: Double): Double = obj1 * l + obj2 * (1.0 - l)
  }

  implicit val byteInterpolator = new ValueInterpolator[Byte] {
    override def blend(obj1: Byte, obj2: Byte, l: Double): Byte = Math.round(obj1 * l + obj2 * (1.0 - l)).toByte
  }

  implicit val uByteInterpolator = new ValueInterpolator[UByte] {
    override def blend(obj1: UByte, obj2: UByte, l: Double): UByte = Scalar[UByte].fromLong(Math.round(obj1.toInt * l + obj2.toInt * (1.0 - l)))
  }

  implicit val shortInterpolator = new ValueInterpolator[Short] {
    override def blend(obj1: Short, obj2: Short, l: Double): Short = Math.round(obj1 * l + obj2 * (1.0 - l)).toShort
  }

  implicit val uShortInterpolator = new ValueInterpolator[UShort] {
    override def blend(obj1: UShort, obj2: UShort, l: Double): UShort = Scalar[UShort].fromLong(Math.round(obj1.toInt * l + obj2.toInt * (1.0 - l)))
  }

  implicit val intInterpolator = new ValueInterpolator[Int] {
    override def blend(obj1: Int, obj2: Int, l: Double): Int = Math.round(obj1 * l + obj2 * (1.0 - l)).toInt
  }

  implicit val uIntInterpolator = new ValueInterpolator[UInt] {
    override def blend(obj1: UInt, obj2: UInt, l: Double): UInt = Scalar[UInt].fromLong(Math.round(obj1.toLong * l + obj2.toLong * (1.0 - l)))
  }

  implicit val longInterpolator = new ValueInterpolator[Long] {
    override def blend(obj1: Long, obj2: Long, l: Double): Long = Math.round(obj1 * l + obj2 * (1.0 - l))
  }

  implicit def pointBlender[D <: Dim] = new ValueInterpolator[Point[D]] {
    override def blend(obj1: Point[D], obj2: Point[D], l: Double): Point[D] = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  implicit val pointBlender1D = new ValueInterpolator[Point1D] {
    override def blend(obj1: Point1D, obj2: Point1D, l: Double): Point1D = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  implicit val pointBlender2D = new ValueInterpolator[Point2D] {
    override def blend(obj1: Point2D, obj2: Point2D, l: Double): Point2D = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  implicit val pointBlender3D = new ValueInterpolator[Point3D] {
    override def blend(obj1: Point3D, obj2: Point3D, l: Double): Point3D = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  // ** VectorXD **

  implicit val vectorBlender1D = new ValueInterpolator[SpatialVector1D] {
    override def blend(obj1: SpatialVector1D, obj2: SpatialVector1D, l: Double): SpatialVector1D = SpatialVector1D(
      obj1.x * l + obj2.x * (1.0 - l))

    override def barycentricInterpolation(v1: SpatialVector1D, f1: Double, v2: SpatialVector1D, f2: Double, v3: SpatialVector1D, f3: Double): SpatialVector1D = {
      SpatialVector1D(
        v1.x * f1 + v2.x * f2 + v3.x * f3)
    }

    override def average(first: SpatialVector1D, rest: SpatialVector1D*): SpatialVector1D = {
      var x: Double = first.x
      rest.foreach { v =>
        x += v.x
      }
      val n = rest.size + 1.0
      SpatialVector1D(x / n)
    }
  }

  implicit val vectorBlender2D = new ValueInterpolator[SpatialVector2D] {
    override def blend(obj1: SpatialVector2D, obj2: SpatialVector2D, l: Double): SpatialVector2D = SpatialVector2D(
      obj1.x * l + obj2.x * (1.0 - l),
      obj1.y * l + obj2.y * (1.0 - l))

    override def barycentricInterpolation(v1: SpatialVector2D, f1: Double, v2: SpatialVector2D, f2: Double, v3: SpatialVector2D, f3: Double): SpatialVector2D = {
      SpatialVector2D(
        v1.x * f1 + v2.x * f2 + v3.x * f3,
        v1.y * f1 + v2.y * f2 + v3.y * f3)
    }

    override def average(first: SpatialVector2D, rest: SpatialVector2D*): SpatialVector2D = {
      var x: Double = first.x
      var y: Double = first.y
      rest.foreach { v =>
        x += v.x
        y += v.y
      }
      val n = rest.size + 1.0
      SpatialVector2D(x / n, y / n)
    }
  }

  implicit val vectorBlender3D = new ValueInterpolator[SpatialVector3D] {
    override def blend(obj1: SpatialVector3D, obj2: SpatialVector3D, l: Double): SpatialVector3D = SpatialVector3D(
      obj1.x * l + obj2.x * (1.0 - l),
      obj1.y * l + obj2.y * (1.0 - l),
      obj1.z * l + obj2.z * (1.0 - l))

    override def barycentricInterpolation(v1: SpatialVector3D, f1: Double, v2: SpatialVector3D, f2: Double, v3: SpatialVector3D, f3: Double): SpatialVector3D = {
      SpatialVector3D(
        v1.x * f1 + v2.x * f2 + v3.x * f3,
        v1.y * f1 + v2.y * f2 + v3.y * f3,
        v1.z * f1 + v2.z * f2 + v3.z * f3)
    }

    override def average(first: SpatialVector3D, rest: SpatialVector3D*): SpatialVector3D = {
      var x: Double = first.x
      var y: Double = first.y
      var z: Double = first.z
      rest.foreach { v =>
        x += v.x
        y += v.y
        z += v.z
      }
      val n = rest.size + 1.0
      SpatialVector3D(x / n, y / n, z / n)
    }
  }

  // ** Vector[D] **

  implicit val vectorBlender_1D = new ValueInterpolator[SpatialVector[_1D]] {
    override def blend(obj1: SpatialVector[_1D], obj2: SpatialVector[_1D], l: Double): SpatialVector[_1D] = vectorBlender1D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: SpatialVector[_1D], f1: Double, v2: SpatialVector[_1D], f2: Double, v3: SpatialVector[_1D], f3: Double): SpatialVector[_1D] = vectorBlender1D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: SpatialVector[_1D], rest: SpatialVector[_1D]*): SpatialVector[_1D] = {
      var x: Double = first.x
      rest.foreach { v =>
        x += v.x
      }
      val n = rest.size + 1.0
      SpatialVector1D(x / n)
    }
  }

  implicit val vectorBlender_2D = new ValueInterpolator[SpatialVector[_2D]] {
    override def blend(obj1: SpatialVector[_2D], obj2: SpatialVector[_2D], l: Double): SpatialVector[_2D] = vectorBlender2D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: SpatialVector[_2D], f1: Double, v2: SpatialVector[_2D], f2: Double, v3: SpatialVector[_2D], f3: Double): SpatialVector[_2D] = vectorBlender2D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: SpatialVector[_2D], rest: SpatialVector[_2D]*): SpatialVector[_2D] = {
      var x: Double = first.x
      var y: Double = first.y
      rest.foreach { v =>
        x += v.x
        y += v.y
      }
      val n = rest.size + 1.0
      SpatialVector2D(x / n, y / n)
    }
  }

  implicit val vectorBlender_3D = new ValueInterpolator[SpatialVector[_3D]] {
    override def blend(obj1: SpatialVector[_3D], obj2: SpatialVector[_3D], l: Double): SpatialVector[_3D] = vectorBlender3D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: SpatialVector[_3D], f1: Double, v2: SpatialVector[_3D], f2: Double, v3: SpatialVector[_3D], f3: Double): SpatialVector[_3D] = vectorBlender3D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: SpatialVector[_3D], rest: SpatialVector[_3D]*): SpatialVector[_3D] = {
      var x: Double = first.x
      var y: Double = first.y
      var z: Double = first.z
      rest.foreach { v =>
        x += v.x
        y += v.y
        z += v.z
      }
      val n = rest.size + 1.0
      SpatialVector3D(x / n, y / n, z / n)
    }
  }
}

