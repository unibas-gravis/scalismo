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
package scalismo.common.interpolation

import scalismo.geometry.Vector._
import scalismo.geometry._

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
      mix = blend(mix, next, n / (n + 1))
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

  implicit val vectorBlender1D = new ValueInterpolator[Vector1D] {
    override def blend(obj1: Vector1D, obj2: Vector1D, l: Double): Vector1D = Vector1D(
      obj1.x * l + obj2.x * (1.0 - l))

    override def barycentricInterpolation(v1: Vector1D, f1: Double, v2: Vector1D, f2: Double, v3: Vector1D, f3: Double): Vector1D = {
      Vector1D(
        v1.x * f1 + v2.x * f2 + v3.x * f3)
    }

    override def average(first: Vector1D, rest: Vector1D*): Vector1D = {
      var x: Double = first.x
      rest.foreach { v =>
        x += v.x
      }
      val n = rest.size + 1.0
      Vector1D(x / n)
    }
  }

  implicit val vectorBlender2D = new ValueInterpolator[Vector2D] {
    override def blend(obj1: Vector2D, obj2: Vector2D, l: Double): Vector2D = Vector2D(
      obj1.x * l + obj2.x * (1.0 - l),
      obj1.y * l + obj2.y * (1.0 - l))

    override def barycentricInterpolation(v1: Vector2D, f1: Double, v2: Vector2D, f2: Double, v3: Vector2D, f3: Double): Vector2D = {
      Vector2D(
        v1.x * f1 + v2.x * f2 + v3.x * f3,
        v1.y * f1 + v2.y * f2 + v3.y * f3)
    }

    override def average(first: Vector2D, rest: Vector2D*): Vector2D = {
      var x: Double = first.x
      var y: Double = first.y
      rest.foreach { v =>
        x += v.x
        y += v.y
      }
      val n = rest.size + 1.0
      Vector2D(x / n, y / n)
    }
  }

  implicit val vectorBlender3D = new ValueInterpolator[Vector3D] {
    override def blend(obj1: Vector3D, obj2: Vector3D, l: Double): Vector3D = Vector3D(
      obj1.x * l + obj2.x * (1.0 - l),
      obj1.y * l + obj2.y * (1.0 - l),
      obj1.z * l + obj2.z * (1.0 - l))

    override def barycentricInterpolation(v1: Vector3D, f1: Double, v2: Vector3D, f2: Double, v3: Vector3D, f3: Double): Vector3D = {
      Vector3D(
        v1.x * f1 + v2.x * f2 + v3.x * f3,
        v1.y * f1 + v2.y * f2 + v3.y * f3,
        v1.z * f1 + v2.z * f2 + v3.z * f3)
    }

    override def average(first: Vector3D, rest: Vector3D*): Vector3D = {
      var x: Double = first.x
      var y: Double = first.y
      var z: Double = first.z
      rest.foreach { v =>
        x += v.x
        y += v.y
        z += v.z
      }
      val n = rest.size + 1.0
      Vector3D(x / n, y / n, z / n)
    }
  }

  // ** Vector[D] **

  implicit val vectorBlender_1D = new ValueInterpolator[Vector[_1D]] {
    override def blend(obj1: Vector[_1D], obj2: Vector[_1D], l: Double): Vector[_1D] = vectorBlender1D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: Vector[_1D], f1: Double, v2: Vector[_1D], f2: Double, v3: Vector[_1D], f3: Double): Vector[_1D] = vectorBlender1D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: Vector[_1D], rest: Vector[_1D]*): Vector[_1D] = {
      var x: Double = first.x
      rest.foreach { v =>
        x += v.x
      }
      val n = rest.size + 1.0
      Vector1D(x / n)
    }
  }

  implicit val vectorBlender_2D = new ValueInterpolator[Vector[_2D]] {
    override def blend(obj1: Vector[_2D], obj2: Vector[_2D], l: Double): Vector[_2D] = vectorBlender2D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: Vector[_2D], f1: Double, v2: Vector[_2D], f2: Double, v3: Vector[_2D], f3: Double): Vector[_2D] = vectorBlender2D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: Vector[_2D], rest: Vector[_2D]*): Vector[_2D] = {
      var x: Double = first.x
      var y: Double = first.y
      rest.foreach { v =>
        x += v.x
        y += v.y
      }
      val n = rest.size + 1.0
      Vector2D(x / n, y / n)
    }
  }

  implicit val vectorBlender_3D = new ValueInterpolator[Vector[_3D]] {
    override def blend(obj1: Vector[_3D], obj2: Vector[_3D], l: Double): Vector[_3D] = vectorBlender3D.blend(obj1, obj2, l)

    override def barycentricInterpolation(v1: Vector[_3D], f1: Double, v2: Vector[_3D], f2: Double, v3: Vector[_3D], f3: Double): Vector[_3D] = vectorBlender3D.barycentricInterpolation(v1, f1, v2, f2, v3, f3)

    override def average(first: Vector[_3D], rest: Vector[_3D]*): Vector[_3D] = {
      var x: Double = first.x
      var y: Double = first.y
      var z: Double = first.z
      rest.foreach { v =>
        x += v.x
        y += v.y
        z += v.z
      }
      val n = rest.size + 1.0
      Vector3D(x / n, y / n, z / n)
    }
  }
}

