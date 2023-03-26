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

import scalismo.geometry.{_2D, _3D, EuclideanVector, Point}
import scalismo.numerics.ValueInterpolator
import scalismo.utils.Random

import scala.annotation.switch

/** barycentric coordinates: localization within triangles */
case class BarycentricCoordinates(a: Double, b: Double, c: Double) {

  def normalized: BarycentricCoordinates = {
    val s = a + b + c
    new BarycentricCoordinates(a / s, b / s, c / s)
  }

  /** perform bcc interpolation: interpolate vertex values within triangle, needs Interpolation[T] */
  def interpolateProperty[@specialized(Float, Double) A](v1: A, v2: A, v3: A)(implicit
    blender: ValueInterpolator[A]
  ): A = {
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

    new BarycentricCoordinates(A1 * x + B1 * y + C1, A2 * x + B2 * y + C2, A3 * x + B3 * y + C3).normalized
  }

  def pointInTriangle3D(point: Point[_3D], v1: Point[_3D], v2: Point[_3D], v3: Point[_3D]): BarycentricCoordinates = {
    val a = v2 - v1
    val b = v3 - v1
    val c = point - v1

    val d00 = a dot a
    val d01 = a dot b
    val d11 = b dot b
    val d20 = c dot a
    val d21 = c dot b
    val d = d00 * d11 - d01 * d01
    val t = (d11 * d20 - d01 * d21) / d
    val s = (d00 * d21 - d01 * d20) / d

    new BarycentricCoordinates(1f - t - s, t, s)
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

/** barycentric coordinates for localization within a tetrahedron */
case class BarycentricCoordinates4(a: Double, b: Double, c: Double, d: Double) {

  def normalized: BarycentricCoordinates4 = {
    val s = a + b + c + d
    new BarycentricCoordinates4(a / s, b / s, c / s, d / s)
  }

  /** perform bcc interpolation: interpolate vertex values within triangle, needs Interpolation[T] */
  def interpolateProperty[@specialized(Float, Double) A](v1: A, v2: A, v3: A, v4: A)(implicit
    blender: ValueInterpolator[A]
  ): A = {
    blender.barycentricInterpolation(v1, a, v2, b, v3, c, v4, d)
  }

  def toArray = Array(a, b, c, d)
}

/** barycentric coordinates: localization within triangles */
object BarycentricCoordinates4 {

  /** coordinates of first vertex */
  val v0 = new BarycentricCoordinates4(1.0, 0.0, 0.0, 0.0)

  /** coordinates of second vertex */
  val v1 = new BarycentricCoordinates4(0.0, 1.0, 0.0, 0.0)

  /** coordinates of third vertex */
  val v2 = new BarycentricCoordinates4(0.0, 0.0, 1.0, 0.0)

  /** coordinates of forth vertex */
  val v3 = new BarycentricCoordinates4(0.0, 0.0, 0.0, 1.0)

  /** coordinates of center point in triangle */
  val center = BarycentricCoordinates4(1.0, 1.0, 1.0, 1.0).normalized

  /** get vertex coordinates, vertexIndex must be 0, 1, 2, or 3 */
  def canonical(vertexIndex: Int): BarycentricCoordinates4 = (vertexIndex: @switch) match {
    case 0 => v0
    case 1 => v1
    case 2 => v2
    case 3 => v3
    case _ => throw new IndexOutOfBoundsException("BarycentricCoordinates4 can only handle 4 vertices: 0-3")
  }

  def pointInTetrahedron(pt: Point[_3D],
                         a: Point[_3D],
                         b: Point[_3D],
                         c: Point[_3D],
                         d: Point[_3D]
  ): BarycentricCoordinates4 = {
    // following https://www.cdsimpson.net/2014/10/barycentric-coordinates.html
    val vap = pt - a
    val vbp = pt - b

    val vab = b - a
    val vac = c - a
    val vad = d - a

    val vbc = c - b
    val vbd = d - b

    def scalarTripleProduct(v1: EuclideanVector[_3D], v2: EuclideanVector[_3D], v3: EuclideanVector[_3D]): Double = {
      v1.dot(v2.crossproduct(v3))
    }

    val va6 = scalarTripleProduct(vbp, vbd, vbc)
    val vb6 = scalarTripleProduct(vap, vac, vad)
    val vc6 = scalarTripleProduct(vap, vad, vab)
    val vd6 = scalarTripleProduct(vap, vab, vac)
    val v6 = 1.0 / scalarTripleProduct(vab, vac, vad)
    BarycentricCoordinates4(va6 * v6, vb6 * v6, vc6 * v6, vd6 * v6)
  }

  /** Generate random barycentric coordinates, guaranteed to lie within the tetrahedron, uniformly distributed */
  def randomUniform(implicit rng: Random): BarycentricCoordinates4 = {
    var s = rng.scalaRandom.nextDouble()
    var t = rng.scalaRandom.nextDouble()
    var u = rng.scalaRandom.nextDouble()

    if (s + t > 1) {
      s = 1.0 - s
      t = 1.0 - t
    }

    if (s + t + u > 1) {
      val tu = u
      if (t + u > 1) {
        u = 1.0 - s - t
        t = 1.0 - tu
      } else {
        u = s + t + u - 1.0
        s = 1.0 - t - tu
      }
    }

    BarycentricCoordinates4(1.0 - s - t - u, s, t, u)
  }
}
