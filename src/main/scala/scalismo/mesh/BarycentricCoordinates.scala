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

import scalismo.geometry.{ Point, _2D }

import scala.annotation.switch
import scala.util.Random

/** barycentric coordinates: localization within triangles */
case class BarycentricCoordinates(a: Float, b: Float, c: Float) {

  def normalized: BarycentricCoordinates = {
    val s = a + b + c
    new BarycentricCoordinates(a / s, b / s, c / s)
  }

  /** perform bcc interpolation: interpolate vertex values within triangle, needs Interpolation[T] */
  def interpolateProperty[@specialized(Float, Double) A](v1: A, v2: A, v3: A)(implicit blender: Interpolator[A]): A = {
    blender.barycentricInterpolation(v1, a, v2, b, v3, c)
  }
}

/** barycentric coordinates: localization within triangles */
object BarycentricCoordinates {

  /** coordinates of first vertex */
  val v0 = new BarycentricCoordinates(1.0f, 0.0f, 0.0f)
  /** coordinates of second vertex */
  val v1 = new BarycentricCoordinates(0.0f, 1.0f, 0.0f)
  /** coordinates of third vertex */
  val v2 = new BarycentricCoordinates(0.0f, 0.0f, 1.0f)

  /** coordinates of center point in triangle */
  val center = BarycentricCoordinates(1.0f, 1.0f, 1.0f).normalized

  /** get vertex coordinates, vertexIndex must be 0, 1 or 2 */
  def canonical(vertexIndex: Int): BarycentricCoordinates = (vertexIndex: @switch) match {
    case 0 => v0
    case 1 => v1
    case 2 => v2
    case _ => throw new IndexOutOfBoundsException("BCC can only handle 3 vertices: 0-2")
  }

  /** find barycentric coordinates for a point within the given triangle */
  def pointInTriangle(point: Point[_2D], v1: Point[_2D], v2: Point[_2D], v3: Point[_2D]): BarycentricCoordinates = {
    val x: Float = point.x
    val y: Float = point.y
    val x1: Float = v1.x
    val y1: Float = v1.y
    val x2: Float = v2.x
    val y2: Float = v2.y
    val x3: Float = v3.x
    val y3: Float = v3.y

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

  /** Generate random barycentric coordinates, guaranteed to lie within the triangle, uniform distribution */
  def randomUniform(seed: Int): BarycentricCoordinates = randomUniform(new Random(seed))

  /** Generate random barycentric coordinates, guaranteed to lie within the triangle, uniform distribution */
  def randomUniform(generator: Random): BarycentricCoordinates = {
    val s = generator.nextFloat()
    val t = generator.nextFloat()
    if (s + t < 1f)
      new BarycentricCoordinates(s, t, 1f - s - t)
    else
      new BarycentricCoordinates(1f - s, 1f - t, s + t - 1f)
  }
}

