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
package scalismo.mesh.boundingSpheres

import breeze.numerics.abs
import scalismo.geometry.{ Vector, _3D }
import scalismo.mesh.boundingSpheres.ClosestPointType._

/**
 * Holds triangles and precalculated vectors.
 */
private[mesh] case class Triangle(a: Vector[_3D], b: Vector[_3D], c: Vector[_3D], ab: Vector[_3D], ac: Vector[_3D], n: Vector[_3D])

/**
 * Barycentric Coordinates. Pair of doubles characterizing a point by the two vectors AB and AC of a triangle.
 */
private case class BC(var a: Double, var b: Double)

/**
 * Collection of helper classes and functions for bounding spheres.
 */
private object BSDistance {

  /**
   * Calculates the barycentric coordinates of a triangle. Returns also the sum of both.
   */
  @inline
  def calculateBarycentricCoordinates(triangle: Triangle, p: Vector[_3D]): (Double, Double, Double) = {
    val x = triangle.a - p
    val ab2 = triangle.ab dot triangle.ab
    val abac = triangle.ab dot triangle.ac
    val ac2 = triangle.ac dot triangle.ac
    val xab = x dot triangle.ab
    val xac = x dot triangle.ac
    val div = ab2 * ac2 - abac * abac
    val s = (abac * xac - ac2 * xab) / div
    val t = (abac * xab - ab2 * xac) / div
    val st = s + t
    (s, t, st)
  }

  // mutable classes
  private[boundingSpheres] case class Index(var idx: Int)
  private[boundingSpheres] case class Distance2(var distance2: Double)
  private[boundingSpheres] case class CP(var distance2: Double, var pt: Vector[_3D], var ptType: ClosestPointType, var bc: BC, var idx: (Int, Int))

  // immutable classes
  private[boundingSpheres] case class DistanceSqr(distance2: Double)
  private[boundingSpheres] case class DistanceSqrAndPoint(distance2: Double, pt: Vector[_3D])

  /**
   * Finds closest point to triangle.
   */
  @inline
  def toTriangle(p: Vector[_3D], triangle: Triangle): ClosestPointMeta = {

    if (abs(triangle.ab(0)) + abs(triangle.ab(1)) + abs(triangle.ab(2)) < 1.0e-12) {
      // Degenerated case where a and b are the same points
      val r = squaredDistanceClosestPointAndBCOnLineSegment(p, triangle.c, triangle.a)
      ClosestPointMeta(r._1, r._2, ON_LINE, (r._3, 0), (2, -1))
    } else if (abs(triangle.ac(0)) + abs(triangle.ac(1)) + abs(triangle.ac(2)) < 1.0e-12) {
      // degenerated case where a and c are the same points
      val r = squaredDistanceClosestPointAndBCOnLineSegment(p, triangle.a, triangle.b)
      ClosestPointMeta(r._1, r._2, ON_LINE, (r._3, 0), (0, -1))
    } else {
      // regular case

      // http://www.geometrictools.com/Documentation/DistancePoint3Triangle3.pdf
      val (s, t, st) = calculateBarycentricCoordinates(triangle, p)

      /* Determine Region that the projected point is in by looking at a and b.
      //     t
      //
      //     ^
      // \ 2 |
      //  \  |
      //   \ |
      //    \|
      //     C
      //     |\
      //     | \
      //     |  \
      //     |   \
      //   3 | 0  \   1
      //     |     \
      //     |      \
      // ----A-------B----------> s
      //     |        \
      //   4 |    5    \   6
      //     |          \
      // Then calculate the distance to the nearest point or line segment.
      */
      if (st < 1.0) {
        if (s > 0) {
          if (t > 0) {
            // region 0
            val nearest = triangle.a + triangle.ab * s + triangle.ac * t
            val dist2 = squaredDistanceToPoint(nearest, p)
            ClosestPointMeta(dist2, nearest, IN_TRIANGLE, (s, t), (-1, -1))
          } else {
            // region 5
            val r = squaredDistanceClosestPointAndBCOnLineSegment(p, triangle.a, triangle.b)
            ClosestPointMeta(r._1, r._2, ON_LINE, (r._3, 0), (0, -1))
          }
        } else {
          if (t > 0) {
            // region 3
            val r = squaredDistanceClosestPointAndBCOnLineSegment(p, triangle.c, triangle.a)
            ClosestPointMeta(r._1, r._2, ON_LINE, (r._3, 0), (2, -1))
          } else {
            // region 4
            val dist2 = squaredDistanceToPoint(triangle.a, p)
            ClosestPointMeta(dist2, triangle.a, POINT, (0.0, 0.0), (0, -1))
          }
        }
      } else {
        if (s > 0) {
          if (t > 0) {
            // region 1
            val r = squaredDistanceClosestPointAndBCOnLineSegment(p, triangle.b, triangle.c)
            ClosestPointMeta(r._1, r._2, ON_LINE, (r._3, 0), (1, -1))
          } else {
            // region 6
            val dist2 = squaredDistanceToPoint(triangle.b, p)
            ClosestPointMeta(dist2, triangle.b, POINT, (1.0, 0.0), (1, -1))
          }
        } else {
          // region 2
          val dist2 = squaredDistanceToPoint(triangle.c, p)
          ClosestPointMeta(dist2, triangle.c, POINT, (0.0, 1.0), (2, -1))
        }
      }

    }
  }

  @inline
  def toLineSegment(p: Vector[_3D], pt1: Vector[_3D], pt2: Vector[_3D]): ClosestPointMeta = {
    val dir = pt2 - pt1 // line direction
    val len2 = dir.norm2
    if (len2 < Double.MinPositiveValue) {
      val nearest = (pt1 + pt2) * 0.5
      ClosestPointMeta(squaredDistanceToPoint(nearest, p), nearest, ON_LINE, (0.5, 0.0), (0, -1))
    } else {
      val s = dir.dot(p - pt1)
      val bc = s / len2
      if (bc > 0.0) {
        if (bc < 1.0) {
          val nearest = pt1 + dir * bc
          ClosestPointMeta(squaredDistanceToPoint(p, nearest), nearest, ON_LINE, (bc, 0.0), (0, -1))
        } else {
          ClosestPointMeta(squaredDistanceToPoint(p, pt2), pt2, POINT, (bc, 0.0), (1, -1))
        }
      } else {
        ClosestPointMeta(squaredDistanceToPoint(p, pt1), pt1, POINT, (bc, 0.0), (0, -1))
      }
    }
  }

  @inline
  def squaredDistanceClosestPointAndBCOnLineSegment(p: Vector[_3D], pt1: Vector[_3D], pt2: Vector[_3D]): (Double, Vector[_3D], Double) = {
    val dir = pt2 - pt1 // line direction
    val len2 = dir.norm2
    if (len2 < Double.MinPositiveValue) {
      val nearest = (pt1 + pt2) * 0.5
      (squaredDistanceToPoint(p, nearest), nearest, 0.5)
    } else {
      val s = dir.dot(p - pt1)
      if (s < 0) {
        (squaredDistanceToPoint(p, pt1), pt1, 0.0)
      } else if (s > len2) {
        (squaredDistanceToPoint(p, pt2), pt2, 1.0)
      } else {
        val bc = s / len2
        val nearest = pt1 + dir * bc
        (squaredDistanceToPoint(p, nearest), nearest, bc)
      }
    }
  }

  @inline
  def squaredDistanceAndClosestPointOnLine(p: Vector[_3D], pt1: Vector[_3D], pt2: Vector[_3D]): (Double, Vector[_3D]) = {
    val dir = (pt2 - pt1).normalize // line direction
    val x = p - pt1 // vector from the point to one point on the line
    val s = dir.dot(x) // length of projection of x onto the line
    val nearest = pt1 + dir * s
    (squaredDistanceToPoint(p, nearest), nearest)
  }

  @inline
  def squaredDistanceToLine(p: Vector[_3D], pt1: Vector[_3D], pt2: Vector[_3D]): Double = {
    val t1 = p - pt1
    val t2 = pt2 - pt1

    val D = t1.crossproduct(t2)

    D.norm2 / t1.norm2
  }

  @inline
  def squaredDistanceToLineDirection(p: Vector[_3D], pointOnLine: Vector[_3D], direction: Vector[_3D]): Double = {
    val v = pointOnLine - p
    (v - direction * (direction.dot(v) / direction.norm2)).norm2
  }

  @inline
  def squaredDistanceToPoint(p: Vector[_3D], pt: Vector[_3D]): Double = {
    (p - pt).norm2
  }

  @inline
  def toPoint(p: Vector[_3D], pt: Vector[_3D]): Distance2 = {
    Distance2((p - pt).norm2)
  }

}
