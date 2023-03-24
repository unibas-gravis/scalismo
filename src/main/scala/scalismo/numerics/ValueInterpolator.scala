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
import scalismo.geometry.EuclideanVector._
import scalismo.geometry._
import spire.math.{UByte, UInt, UShort}

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

  /** fast explicit barycentric interpolation, most used case of multiple blends */
  def barycentricInterpolation(v1: A, f1: Double, v2: A, f2: Double, v3: A, f3: Double, v4: A, f4: Double): A = {
    val f12 = f1 + f2
    val f123 = f12 + f3
    if (f123 > 0) {
      blend(
        if (f12 > 0) {
          blend(
            blend(v1, v2, f1 / f12),
            v3,
            f12 / (f3 + f12)
          )
        } else {
          v3
        },
        v4,
        f123 / (f4 + f123)
      )
    } else {
      v4
    }
  }

  /**
   * direct access to averaging function, warning: unstable for large sequences! (implement hierarchical blending for
   * better stability)
   */
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

  implicit val floatInterpolator: ValueInterpolator[Float] = new ValueInterpolator[Float] {
    override def blend(obj1: Float, obj2: Float, l: Double): Float = (obj1 * l + obj2 * (1.0 - l)).toFloat
  }

  implicit val doubleInterpolator: ValueInterpolator[Double] = new ValueInterpolator[Double] {
    override def blend(obj1: Double, obj2: Double, l: Double): Double = obj1 * l + obj2 * (1.0 - l)
  }

  implicit val byteInterpolator: ValueInterpolator[Byte] = new ValueInterpolator[Byte] {
    override def blend(obj1: Byte, obj2: Byte, l: Double): Byte = Math.round(obj1 * l + obj2 * (1.0 - l)).toByte
  }

  implicit val uByteInterpolator: ValueInterpolator[UByte] = new ValueInterpolator[UByte] {
    override def blend(obj1: UByte, obj2: UByte, l: Double): UByte =
      Scalar[UByte].fromLong(Math.round(obj1.toInt * l + obj2.toInt * (1.0 - l)))
  }

  implicit val shortInterpolator: ValueInterpolator[Short] = new ValueInterpolator[Short] {
    override def blend(obj1: Short, obj2: Short, l: Double): Short = Math.round(obj1 * l + obj2 * (1.0 - l)).toShort
  }

  implicit val uShortInterpolator: ValueInterpolator[UShort] = new ValueInterpolator[UShort] {
    override def blend(obj1: UShort, obj2: UShort, l: Double): UShort =
      Scalar[UShort].fromLong(Math.round(obj1.toInt * l + obj2.toInt * (1.0 - l)))
  }

  implicit val intInterpolator: ValueInterpolator[Int] = new ValueInterpolator[Int] {
    override def blend(obj1: Int, obj2: Int, l: Double): Int = Math.round(obj1 * l + obj2 * (1.0 - l)).toInt
  }

  implicit val uIntInterpolator: ValueInterpolator[UInt] = new ValueInterpolator[UInt] {
    override def blend(obj1: UInt, obj2: UInt, l: Double): UInt =
      Scalar[UInt].fromLong(Math.round(obj1.toLong * l + obj2.toLong * (1.0 - l)))
  }

  implicit val longInterpolator: ValueInterpolator[Long] = new ValueInterpolator[Long] {
    override def blend(obj1: Long, obj2: Long, l: Double): Long = Math.round(obj1 * l + obj2 * (1.0 - l))
  }

  implicit def pointBlender[D]: ValueInterpolator[Point[D]] = new ValueInterpolator[Point[D]] {
    override def blend(obj1: Point[D], obj2: Point[D], l: Double): Point[D] = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  implicit val pointBlender1D: ValueInterpolator[Point1D] = new ValueInterpolator[Point1D] {
    override def blend(obj1: Point1D, obj2: Point1D, l: Double): Point1D = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  implicit val pointBlender2D: ValueInterpolator[Point2D] = new ValueInterpolator[Point2D] {
    override def blend(obj1: Point2D, obj2: Point2D, l: Double): Point2D = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  implicit val pointBlender3D: ValueInterpolator[Point3D] = new ValueInterpolator[Point3D] {
    override def blend(obj1: Point3D, obj2: Point3D, l: Double): Point3D = obj1 + (1.0 - l) *: (obj2 - obj1)
  }

  // ** VectorXD **

  implicit val vectorBlender1D: ValueInterpolator[EuclideanVector1D] = new ValueInterpolator[EuclideanVector1D] {
    override def blend(obj1: EuclideanVector1D, obj2: EuclideanVector1D, l: Double): EuclideanVector1D =
      EuclideanVector1D(obj1.x * l + obj2.x * (1.0 - l))

    override def barycentricInterpolation(v1: EuclideanVector1D,
                                          f1: Double,
                                          v2: EuclideanVector1D,
                                          f2: Double,
                                          v3: EuclideanVector1D,
                                          f3: Double
    ): EuclideanVector1D = {
      EuclideanVector1D(v1.x * f1 + v2.x * f2 + v3.x * f3)
    }

    override def average(first: EuclideanVector1D, rest: EuclideanVector1D*): EuclideanVector1D = {
      var x: Double = first.x
      rest.foreach { v =>
        x += v.x
      }
      val n = rest.size + 1.0
      EuclideanVector1D(x / n)
    }
  }

  implicit val vectorBlender2D: ValueInterpolator[EuclideanVector2D] = new ValueInterpolator[EuclideanVector2D] {
    override def blend(obj1: EuclideanVector2D, obj2: EuclideanVector2D, l: Double): EuclideanVector2D =
      EuclideanVector2D(obj1.x * l + obj2.x * (1.0 - l), obj1.y * l + obj2.y * (1.0 - l))

    override def barycentricInterpolation(v1: EuclideanVector2D,
                                          f1: Double,
                                          v2: EuclideanVector2D,
                                          f2: Double,
                                          v3: EuclideanVector2D,
                                          f3: Double
    ): EuclideanVector2D = {
      EuclideanVector2D(v1.x * f1 + v2.x * f2 + v3.x * f3, v1.y * f1 + v2.y * f2 + v3.y * f3)
    }

    override def average(first: EuclideanVector2D, rest: EuclideanVector2D*): EuclideanVector2D = {
      var x: Double = first.x
      var y: Double = first.y
      rest.foreach { v =>
        x += v.x
        y += v.y
      }
      val n = rest.size + 1.0
      EuclideanVector2D(x / n, y / n)
    }
  }

  implicit val vectorBlender3D: ValueInterpolator[EuclideanVector3D] = new ValueInterpolator[EuclideanVector3D] {
    override def blend(obj1: EuclideanVector3D, obj2: EuclideanVector3D, l: Double): EuclideanVector3D =
      EuclideanVector3D(obj1.x * l + obj2.x * (1.0 - l),
                        obj1.y * l + obj2.y * (1.0 - l),
                        obj1.z * l + obj2.z * (1.0 - l)
      )

    override def barycentricInterpolation(v1: EuclideanVector3D,
                                          f1: Double,
                                          v2: EuclideanVector3D,
                                          f2: Double,
                                          v3: EuclideanVector3D,
                                          f3: Double
    ): EuclideanVector3D = {
      EuclideanVector3D(v1.x * f1 + v2.x * f2 + v3.x * f3,
                        v1.y * f1 + v2.y * f2 + v3.y * f3,
                        v1.z * f1 + v2.z * f2 + v3.z * f3
      )
    }

    override def average(first: EuclideanVector3D, rest: EuclideanVector3D*): EuclideanVector3D = {
      var x: Double = first.x
      var y: Double = first.y
      var z: Double = first.z
      rest.foreach { v =>
        x += v.x
        y += v.y
        z += v.z
      }
      val n = rest.size + 1.0
      EuclideanVector3D(x / n, y / n, z / n)
    }
  }

  // ** Vector[D] **

  implicit val vectorBlender_1D: ValueInterpolator[EuclideanVector[_1D]] = new ValueInterpolator[EuclideanVector[_1D]] {
    override def blend(obj1: EuclideanVector[_1D], obj2: EuclideanVector[_1D], l: Double): EuclideanVector[_1D] =
      vectorBlender1D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: EuclideanVector[_1D],
                                          f1: Double,
                                          v2: EuclideanVector[_1D],
                                          f2: Double,
                                          v3: EuclideanVector[_1D],
                                          f3: Double
    ): EuclideanVector[_1D] =
      vectorBlender1D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: EuclideanVector[_1D], rest: EuclideanVector[_1D]*): EuclideanVector[_1D] = {
      var x: Double = first.x
      rest.foreach { v =>
        x += v.x
      }
      val n = rest.size + 1.0
      EuclideanVector1D(x / n)
    }
  }

  implicit val vectorBlender_2D: ValueInterpolator[EuclideanVector[_2D]] = new ValueInterpolator[EuclideanVector[_2D]] {
    override def blend(obj1: EuclideanVector[_2D], obj2: EuclideanVector[_2D], l: Double): EuclideanVector[_2D] =
      vectorBlender2D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: EuclideanVector[_2D],
                                          f1: Double,
                                          v2: EuclideanVector[_2D],
                                          f2: Double,
                                          v3: EuclideanVector[_2D],
                                          f3: Double
    ): EuclideanVector[_2D] =
      vectorBlender2D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: EuclideanVector[_2D], rest: EuclideanVector[_2D]*): EuclideanVector[_2D] = {
      var x: Double = first.x
      var y: Double = first.y
      rest.foreach { v =>
        x += v.x
        y += v.y
      }
      val n = rest.size + 1.0
      EuclideanVector2D(x / n, y / n)
    }
  }

  implicit val vectorBlender_3D: ValueInterpolator[EuclideanVector[_3D]] = new ValueInterpolator[EuclideanVector[_3D]] {
    override def blend(obj1: EuclideanVector[_3D], obj2: EuclideanVector[_3D], l: Double): EuclideanVector[_3D] =
      vectorBlender3D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: EuclideanVector[_3D],
                                          f1: Double,
                                          v2: EuclideanVector[_3D],
                                          f2: Double,
                                          v3: EuclideanVector[_3D],
                                          f3: Double
    ): EuclideanVector[_3D] =
      vectorBlender3D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: EuclideanVector[_3D], rest: EuclideanVector[_3D]*): EuclideanVector[_3D] = {
      var x: Double = first.x
      var y: Double = first.y
      var z: Double = first.z
      rest.foreach { v =>
        x += v.x
        y += v.y
        z += v.z
      }
      val n = rest.size + 1.0
      EuclideanVector3D(x / n, y / n, z / n)
    }
  }
}
