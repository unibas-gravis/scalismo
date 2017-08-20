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
package scalismo.mesh

import scalismo.common.interpolation.ValueInterpolator
import scalismo.geometry.{ Point, Vector, _2D, _3D }
import scalismo.utils.Random

import scala.annotation.switch

/** barycentric coordinates: localization within triangles */
case class BarycentricCoordinates(a: Double, b: Double, c: Double) {

  def normalized: BarycentricCoordinates = {
    val s = a + b + c
    new BarycentricCoordinates(a / s, b / s, c / s)
  }

  /** perform bcc interpolation: interpolate vertex values within triangle, needs Interpolation[T] */
  def interpolateProperty[@specialized(Float, Double) A](v1: A, v2: A, v3: A)(implicit blender: ValueInterpolator[A]): A = {
    blender.barycentricInterpolation(v1, a, v2, b, v3, c)
  }
}

/** barycentric coordinates: localization within triangles */
object BarycentricCoordinates {

  /** coordinates of first vertex */
  val v0 = new BarycentricCoordinates(1.0, 0.0, 0.0)
  /** coordinates of second vertex */
  val v1 = new BarycentricCoordinates(0.0, 1.0, 0.0)
  /** coordinates of third vertex */
  val v2 = new BarycentricCoordinates(0.0, 0.0, 1.0)

  /** coordinates of center point in triangle */
  val center = BarycentricCoordinates(1.0, 1.0, 1.0).normalized

  /** get vertex coordinates, vertexIndex must be 0, 1 or 2 */
  def canonical(vertexIndex: Int): BarycentricCoordinates = (vertexIndex: @switch) match {
    case 0 => v0
    case 1 => v1
    case 2 => v2
    case _ => throw new IndexOutOfBoundsException("BCC can only handle 3 vertices: 0-2")
  }

  /** find barycentric coordinates for a point within the given triangle */
  def pointInTriangle(point: Point[_2D], v1: Point[_2D], v2: Point[_2D], v3: Point[_2D]): BarycentricCoordinates = {
    val x: Double = point.x
    val y: Double = point.y
    val x1: Double = v1.x
    val y1: Double = v1.y
    val x2: Double = v2.x
    val y2: Double = v2.y
    val x3: Double = v3.x
    val y3: Double = v3.y

    val A1 = y2 - y3
    val A2 = y3 - y1
    val A3 = y1 - y2

    val B1 = x3 - x2
    val B2 = x1 - x3
    val B3 = x2 - x1

    val C1 = x2 * y3 - x3 * y2
    val C2 = x3 * y1 - x1 * y3
    val C3 = x1 * y2 - x2 * y1

    new BarycentricCoordinates(
      A1 * x + B1 * y + C1,
      A2 * x + B2 * y + C2,
      A3 * x + B3 * y + C3
    ).normalized
  }

  def pointInTriangle3D(point: Point[_3D], v1: Point[_3D], v2: Point[_3D], v3: Point[_3D]): BarycentricCoordinates = {
    val a: Vector[_3D] = v2 - v1
    val b: Vector[_3D] = v3 - v1
    val c: Vector[_3D] = point - v1
    val d00 = a dot a
    val d01 = a dot b
    val d11 = b dot b
    val d20 = c dot a
    val d21 = c dot b
    val d = d00 * d11 - d01 * d01
    val t = (d11 * d20 - d01 * d21) / d
    val s = (d00 * d21 - d01 * d20) / d
    new BarycentricCoordinates(s, t, 1f - t - s)
  }

  /** Generate random barycentric coordinates, guaranteed to lie within the triangle, uniform distribution */
  def randomUniform(implicit random: Random): BarycentricCoordinates = {
    val s = random.scalaRandom.nextDouble()
    val t = random.scalaRandom.nextDouble()
    if (s + t < 1.0)
      new BarycentricCoordinates(s, t, 1.0 - s - t)
    else
      new BarycentricCoordinates(1.0 - s, 1.0 - t, s + t - 1.0)
  }
}

