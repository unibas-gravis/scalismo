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
import scalismo.geometry.{EuclideanVector, _3D}
import scalismo.mesh.TriangleCell
import scalismo.mesh.boundingSpheres.ClosestPointType._

/**
  * Holds triangles and precalculated vectors.
  */
private[mesh] case class Triangle(a: EuclideanVector[_3D], b: EuclideanVector[_3D], c: EuclideanVector[_3D]) {
  val ab = b - a
  val ac = c - a
  val bc = c - b
  val n = ab.crossproduct(ac)

  val degenerated = if (n.norm == 0.0) { if (a == b && b == c) 2 else 1 } else 0
  // 0: ab, 1: ac, 2: bc
  val longestSide = {
    val bc = c - b
    if (ab.norm2 > ac.norm2) {
      if (ab.norm2 > bc.norm2) 0 else 2
    } else {
      if (ac.norm2 > bc.norm2) 1 else 2
    }
  }
}



/**
  * Holds tetrahedron and precalculated vectors.
  */
case class Tetrahedron(a: EuclideanVector[_3D], b: EuclideanVector[_3D], c: EuclideanVector[_3D],d:EuclideanVector[_3D]) {
  val ab = b - a
  val ac = c - a
  val ad = d - a
  val bc = c - b
  val bd = d - b
  val cd = d - c

  val n1 = ab.crossproduct(ac)
  val n2 = ab.crossproduct(ad)
  val n3 = ad.crossproduct(ac)
  val n4 = bc.crossproduct(bd)

  val degenerated = if (n1.norm == 0.0) {
    0
  } else if (n2.norm == 0.0){
    1
  }else if (n3.norm == 0.0){
    2
  }else if (n3.norm == 0.0){
    3
  }
  // 0: abc, 1: abd, 2: adc, 3:bcd


  val triangles = List(Triangle(a,b,c),Triangle(a,b,d),Triangle(a,c,d),Triangle(b,c,d))
def largestFace():Int= {
  def computeTriangleArea(A: EuclideanVector[_3D], B: EuclideanVector[_3D], C: EuclideanVector[_3D]): Double = {
    // compute are of the triangle using heron's formula
    val a = (B - A).norm
    val b = (C - B).norm
    val c = (C - A).norm
    val s = (a + b + c) / 2
    val areaSquared = s * (s - a) * (s - b) * (s - c)
    // it can happen that the area is negative, due to a degenerate triangle.
    if (areaSquared <= 0.0) 0.0 else math.sqrt(areaSquared)
  }

  val sq = IndexedSeq(computeTriangleArea(a, b, c), computeTriangleArea(a, b, d), computeTriangleArea(a, d, c), computeTriangleArea(b, c, d))

  var larg =(0,0.0)

  val largestFace = for (i <- 0 to sq.size - 1) {
    if (sq(i)>larg._2){
      larg=(i,sq(i))
    }
  }

   larg._1
  }

}
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
  def calculateBarycentricCoordinates(triangle: Triangle, p: EuclideanVector[_3D]): (Double, Double, Double) = {
    if (triangle.degenerated == 2) {
      (1.0, 0, 0)
    } else if (triangle.degenerated == 1) {
      triangle.longestSide match {
        case 0 =>
          val s = triangle.ab.normalize.dot(p - triangle.a)
          val coordinate = s / triangle.ab.norm
          (1 - coordinate, coordinate, 0)
        case 1 =>
          val s = triangle.ac.normalize.dot(p - triangle.a)
          val cooridnate = s / triangle.ac.norm
          (1 - cooridnate, 0, cooridnate)
        case 2 =>
          val s = triangle.bc.normalize.dot(p - triangle.b)
          val cooridnate = s / triangle.bc.norm
          (0.0, 1 - cooridnate, cooridnate)
      }
    } else {
      val positionRelativeToA = triangle.a - p
      val ab2 = triangle.ab dot triangle.ab
      val abac = triangle.ab dot triangle.ac
      val ac2 = triangle.ac dot triangle.ac
      val xab = positionRelativeToA dot triangle.ab
      val xac = positionRelativeToA dot triangle.ac
      val div = ab2 * ac2 - abac * abac
      val s = (abac * xac - ac2 * xab) / div
      val t = (abac * xab - ab2 * xac) / div
      val st = s + t
      (s, t, st)
    }
  }

  // mutable classes
  private[boundingSpheres] case class Index(var idx: Int)
  private[boundingSpheres] case class Distance2(var distance2: Double)
  private[boundingSpheres] case class CP(var distance2: Double, var pt: EuclideanVector[_3D], var ptType: ClosestPointType, var bc: BC, var idx: (Int, Int))

  // immutable classes

  private[boundingSpheres] case class DistanceSqr(val distance2: Double)
  private[boundingSpheres] case class DistanceSqrAndPoint(val distance2: Double, pt: EuclideanVector[_3D])

  /**
   * Finds closest point to triangle.
   */
  @inline
  def toTriangle(p: EuclideanVector[_3D], triangle: Triangle): ClosestPointMeta = {

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
  def toLineSegment(p: EuclideanVector[_3D], pt1: EuclideanVector[_3D], pt2: EuclideanVector[_3D]): ClosestPointMeta = {
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
  def squaredDistanceClosestPointAndBCOnLineSegment(p: EuclideanVector[_3D], pt1: EuclideanVector[_3D], pt2: EuclideanVector[_3D]): (Double, EuclideanVector[_3D], Double) = {
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
  def squaredDistanceAndClosestPointOnLine(p: EuclideanVector[_3D], pt1: EuclideanVector[_3D], pt2: EuclideanVector[_3D]): (Double, EuclideanVector[_3D]) = {
    val dir = (pt2 - pt1).normalize // line direction
    val x = p - pt1 // vector from the point to one point on the line
    val s = dir.dot(x) // length of projection of x onto the line
    val nearest = pt1 + dir * s
    (squaredDistanceToPoint(p, nearest), nearest)
  }

  @inline
  def squaredDistanceToLine(p: EuclideanVector[_3D], pt1: EuclideanVector[_3D], pt2: EuclideanVector[_3D]): Double = {
    val t1 = p - pt1
    val t2 = pt2 - pt1

    val D = t1.crossproduct(t2)

    D.norm2 / t1.norm2
  }

  @inline
  def squaredDistanceToLineDirection(p: EuclideanVector[_3D], pointOnLine: EuclideanVector[_3D], direction: EuclideanVector[_3D]): Double = {
    val v = pointOnLine - p
    (v - direction * (direction.dot(v) / direction.norm2)).norm2
  }

  @inline
  def squaredDistanceToPoint(p: EuclideanVector[_3D], pt: EuclideanVector[_3D]): Double = {
    (p - pt).norm2
  }

  @inline
  def toPoint(p: EuclideanVector[_3D], pt: EuclideanVector[_3D]): Distance2 = {
    Distance2((p - pt).norm2)
  }

}
