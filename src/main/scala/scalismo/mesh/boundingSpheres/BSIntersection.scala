/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.mesh.boundingSpheres

import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.mesh.{BarycentricCoordinates, BarycentricCoordinates4}
import scalismo.numerics.Determinant

/**
 * Helper function to calculate intersection points.
 */
private[boundingSpheres] object BSIntersection {

  def intersectLineWithTriangle(point: EuclideanVector[_3D],
                                direction: EuclideanVector[_3D],
                                a: EuclideanVector[_3D],
                                b: EuclideanVector[_3D],
                                c: EuclideanVector[_3D]
  ): (Boolean, Point[_3D]) = {
    val det = Determinant.det3x3(a.x - b.x,
                                 a.x - c.x,
                                 direction.x,
                                 a.y - b.y,
                                 a.y - c.y,
                                 direction.y,
                                 a.z - b.z,
                                 a.z - c.z,
                                 direction.z
    )

    val beta = Determinant.det3x3(a.x - point.x,
                                  a.x - c.x,
                                  direction.x,
                                  a.y - point.y,
                                  a.y - c.y,
                                  direction.y,
                                  a.z - point.z,
                                  a.z - c.z,
                                  direction.z
    ) / det

    val gamma = Determinant.det3x3(a.x - b.x,
                                   a.x - point.x,
                                   direction.x,
                                   a.y - b.y,
                                   a.y - point.y,
                                   direction.y,
                                   a.z - b.z,
                                   a.z - point.z,
                                   direction.z
    ) / det

    val t = Determinant.det3x3(a.x - b.x,
                               a.x - c.x,
                               a.x - point.x,
                               a.y - b.y,
                               a.y - c.y,
                               a.y - point.y,
                               a.z - b.z,
                               a.z - c.z,
                               a.z - point.z
    ) / det

    if (beta >= 0.0 && gamma >= 0.0 && beta + gamma <= 1.0) {
      (true, Point(point.x + t * direction.x, point.y + t * direction.y, point.z + t * direction.z))
    } else {
      (false, Point(-1, -1, -1))
    }

  }

  def intersectLineWithTriangleBarycentric(point: EuclideanVector[_3D],
                                           direction: EuclideanVector[_3D],
                                           a: EuclideanVector[_3D],
                                           b: EuclideanVector[_3D],
                                           c: EuclideanVector[_3D]
  ): (Boolean, BarycentricCoordinates) = {
    val det = Determinant.det3x3(a.x - b.x,
                                 a.x - c.x,
                                 direction.x,
                                 a.y - b.y,
                                 a.y - c.y,
                                 direction.y,
                                 a.z - b.z,
                                 a.z - c.z,
                                 direction.z
    )

    val beta = Determinant.det3x3(a.x - point.x,
                                  a.x - c.x,
                                  direction.x,
                                  a.y - point.y,
                                  a.y - c.y,
                                  direction.y,
                                  a.z - point.z,
                                  a.z - c.z,
                                  direction.z
    ) / det

    val gamma = Determinant.det3x3(a.x - b.x,
                                   a.x - point.x,
                                   direction.x,
                                   a.y - b.y,
                                   a.y - point.y,
                                   direction.y,
                                   a.z - b.z,
                                   a.z - point.z,
                                   direction.z
    ) / det

    if (beta >= 0.0 && gamma >= 0.0 && beta + gamma <= 1.0) {
      (true, BarycentricCoordinates(1 - beta - gamma, beta, gamma))
    } else {
      (false, BarycentricCoordinates(-1, -1, -1))
    }
  }

  def intersectLineWithTetrahedron(point: EuclideanVector[_3D],
                                   direction: EuclideanVector[_3D],
                                   a: EuclideanVector[_3D],
                                   b: EuclideanVector[_3D],
                                   c: EuclideanVector[_3D],
                                   d: EuclideanVector[_3D]
  ): (Boolean, Seq[Point[_3D]]) = {
    val intersections = IndexedSeq(
      intersectLineWithTriangle(point, direction, a, b, c),
      intersectLineWithTriangle(point, direction, a, d, b),
      intersectLineWithTriangle(point, direction, a, c, d),
      intersectLineWithTriangle(point, direction, b, d, c)
    ).filter(_._1)

    if (intersections.isEmpty) {
      (false, Nil)
    } else {
      (true, intersections.map(_._2))
    }
  }

  def intersectLineWithTetrahedronBarycentric(
    point: EuclideanVector[_3D],
    direction: EuclideanVector[_3D],
    a: EuclideanVector[_3D],
    b: EuclideanVector[_3D],
    c: EuclideanVector[_3D],
    d: EuclideanVector[_3D]
  ): (Boolean, List[BarycentricCoordinates4]) = {
    val intersections = List(
      intersectLineWithTriangle(point, direction, a, b, c),
      intersectLineWithTriangle(point, direction, a, d, b),
      intersectLineWithTriangle(point, direction, a, c, d),
      intersectLineWithTriangle(point, direction, b, d, c)
    ).filter(_._1)

    if (intersections.isEmpty) {
      (false, Nil)
    } else {
      (true, intersections.map(e => calculateBarycentricCoordinates(e._2.toVector, a, b, c, d)))
    }
  }

  @inline
  def calculateBarycentricCoordinates(
    p: EuclideanVector[_3D],
    a: EuclideanVector[_3D],
    b: EuclideanVector[_3D],
    c: EuclideanVector[_3D],
    d: EuclideanVector[_3D]
  ): BarycentricCoordinates4 = {
    val vap = p - a

    val vab = b - a
    val vac = c - a
    val vad = d - a

    def scalarTripleProduct(v1: EuclideanVector[_3D], v2: EuclideanVector[_3D], v3: EuclideanVector[_3D]): Double = {
      v1.dot(v2.crossproduct(v3))
    }

    val vb6 = scalarTripleProduct(vap, vac, vad)
    val vc6 = scalarTripleProduct(vap, vad, vab)
    val vd6 = scalarTripleProduct(vap, vab, vac)
    val v6 = 1.0 / scalarTripleProduct(vab, vac, vad)
    val s = vb6 * v6
    val t = vc6 * v6
    val u = vd6 * v6
    BarycentricCoordinates4(1.0 - (s + t + u), s, t, u)
  }

  def intersectLineSphereSquared(point: EuclideanVector[_3D],
                                 direction: EuclideanVector[_3D],
                                 center: EuclideanVector[_3D],
                                 r2: Double
  ): Boolean = {
    BSDistance.squaredDistanceToLineDirection(center, point, direction) < r2
  }

}
