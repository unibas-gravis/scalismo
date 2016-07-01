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
package scalismo.mesh.surfaceDistance

import breeze.linalg.max
import breeze.numerics.{abs, pow, sqrt}
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.mesh.TriangleMesh3D
import scalismo.mesh.surfaceDistance.BSDistance.Triangle

import scala.annotation.tailrec


/**
  * Bounding sphere node of the tree structure.
  *
  * @center Center of bounding sphere.
  * @r2 Squared radius of bounding sphere.
  * @idx Index of entity used to form leave.
  */
abstract class BoundingSphere(val center: Vector[_3D],
                              val r2: Double,
                              val idx: Int,
                              val left: BoundingSphere,
                              val right: BoundingSphere) {
  def hasLeft: Boolean

  def hasRight: Boolean
}


/**
  * Factory for the BoundingSphere search structure.
  */
object BoundingSpheres {

  def createForPoints(pointList: Seq[Point[_3D]]): BoundingSphere = {
    val leaves = Sphere.fromPoints(pointList)
    val root = buildSpheres(leaves)
    root
  }

  def createForTriangles(triangleList: Seq[Triangle]): BoundingSphere = {
    val leaves = Sphere.fromTriangles(triangleList)
    val root = buildSpheres(leaves)
    root
  }

  private[surfaceDistance] def buildSpheres(leaves: IndexedSeq[Sphere]): BoundingSphere = {
    val binarySphereLeaves = leaves.zipWithIndex.map(l => new BoundingSphereLeave(l._1.center, l._1.r2, l._2))
    buildTree(binarySphereLeaves)
  }


  private[surfaceDistance] final def buildTree(partitions: IndexedSeq[BoundingSphere]): BoundingSphere = {
    partitions.length match {
      case 1 =>
        partitions(0)

      case _ =>
        val centers = partitions.map(s => s.center)
        val nearestPointIndex = calculateNearestPointPairs(centers)
        val subtrees = mergeNearestSpherePairs(partitions, nearestPointIndex)
        buildTree(subtrees)
    }
  }


  def mergeNearestSpherePairs(partitions: IndexedSeq[BoundingSphere], nearestPointIndex: IndexedSeq[Int]): IndexedSeq[BoundingSphere] = {
    val used = Array.fill[Boolean](nearestPointIndex.length)(false)
    val mergedTrees = nearestPointIndex.zipWithIndex.map { p =>
      if (p._1 != p._2) {
        // do not merge sphere with itself
        if (!used(p._1) && !used(p._2)) {
          // do not merge used spheres
          used(p._1) = true
          used(p._2) = true
          Option(createSubtree(partitions(p._1), partitions(p._2)))
        } else {
          None
        }
      } else {
        None
      }
    }
    val unmergedTrees = used.zipWithIndex.filter(u => !u._1).map(u => partitions(u._2))
    val subtrees = mergedTrees.flatten ++ unmergedTrees
    subtrees
  }


  private[surfaceDistance] def createSubtree(a: BoundingSphere, b: BoundingSphere): BoundingSphere = {
    val ab = b.center - a.center
    val dist2 = ab.norm2

    val (newCenter, newRadius) = if (dist2 < Double.MinPositiveValue) {
      // both have same center
      (a.center, max(a.r2, b.r2) + Double.MinPositiveValue)
    } else {
      // both have different center
      // calculate oposite points of spheres
      val na = a.center - ab * sqrt(a.r2 / dist2)
      val nb = b.center + ab * sqrt(b.r2 / dist2)
      val newCenter = (na + nb) * 0.5
      // val newRadius = ((na - nb) / 2).norm2 // @note: this is numerically unstable
      val newRadius = pow(max(
        (newCenter - a.center).norm + sqrt(a.r2), // numerically more stable
        (newCenter - b.center).norm + sqrt(b.r2)
      ), 2)
      (newCenter, newRadius)
    }
    new BoundingSphereSplit(newCenter, newRadius, -1, a, b)
  }


  private[surfaceDistance] def calculateNearestPointPairs(points: IndexedSeq[Vector[_3D]]): IndexedSeq[Int] = {
    val matchedPoints = Array.fill[Int](points.length)(-1)
    val pointsWithIndex = points.zipWithIndex

    matchPoints(pointsWithIndex, matchedPoints)
    matchedPoints.toIndexedSeq
  }

  @tailrec
  private[surfaceDistance] final def matchPoints(points: IndexedSeq[(Vector[_3D], Int)], matchedPoints: Array[Int]): Unit = {
    points.length match {

      case 0 =>

      case 1 =>
        val p = points.head
        matchedPoints(p._2) = p._2

      case _ =>
        val sortedPoints = points.sortBy(_._1(1))
        val closestPointPairs = findClosestPointPairs(sortedPoints)

        val chosen: Array[Boolean] = choosePointPairsAndUpdateMatchedIndex(closestPointPairs, sortedPoints, matchedPoints)
        val stillActive = chosen.zipWithIndex.filter(s => !s._1).map(t => sortedPoints(t._2))
        matchPoints(stillActive, matchedPoints)

    }
  }

  @inline
  private[surfaceDistance] def choosePointPairsAndUpdateMatchedIndex(closestPointPairs: IndexedSeq[(Double, Int, ((Vector[_3D], Int), Int))],
                                                                     sortedPoints: IndexedSeq[(Vector[_3D], Int)],
                                                                     matchedPoints: Array[Int]
                                                                    ): Array[Boolean] = {
    val chosen = Array.fill[Boolean](closestPointPairs.length)(false)
    val bestPairs = closestPointPairs.sortBy(a => a._1)
    bestPairs.foreach {
      cp =>
        val bestSortedPointIndex = cp._2
        val sortedPointIdx = cp._3._2
        val pointIdx = sortedPoints(sortedPointIdx)._2
        val bestPointIndex = sortedPoints(bestSortedPointIndex)._2

        if (!chosen(sortedPointIdx) && !chosen(bestSortedPointIndex)) {
          matchedPoints(pointIdx) = bestPointIndex
          matchedPoints(bestPointIndex) = pointIdx
          chosen(sortedPointIdx) = true
          chosen(bestSortedPointIndex) = true
        }
    }
    chosen
  }

  @inline
  private[surfaceDistance] def findClosestPointPairs(sortedPoints: IndexedSeq[(Vector[_3D], Int)]) = {
    sortedPoints.zipWithIndex.map {
      e =>
        val spIndex = e._2
        val basePoint = e._1._1

        var bestIndex = (spIndex + 1) % sortedPoints.length
        var d = (basePoint - sortedPoints(bestIndex)._1).norm2
        ((spIndex + 2) until sortedPoints.length).takeWhile {
          j =>
            val runningPoint = sortedPoints(j)._1
            val q = basePoint(1) - runningPoint(1)
            if ((q * q) < d) {
              // early stopping according to y difference
              val t = (basePoint - runningPoint).norm2
              if (t < d) {
                d = t
                bestIndex = j
              }
              true
            } else {
              false
            }
        }

        ((spIndex - 1) to 0 by -1).takeWhile {
          j =>
            val runningPoint = sortedPoints(j)._1
            val q = basePoint(1) - runningPoint(1)
            if ((q * q) < d) {
              val t = (basePoint - runningPoint).norm2
              if (t < d) {
                d = t
                bestIndex = j
              }
              true
            } else {
              false
            }
        }

        (d, bestIndex, e)
    }
  }
}


private class BoundingSphereSplit(center: Vector[_3D],
                                  r2: Double,
                                  idx: Int,
                                  left: BoundingSphere,
                                  right: BoundingSphere
                                 ) extends BoundingSphere(center, r2, idx, left, right) {
  override def hasLeft: Boolean = left != null

  override def hasRight: Boolean = right != null
}

private class BoundingSphereLeave(center: Vector[_3D],
                                  r2: Double,
                                  idx: Int
                                 ) extends BoundingSphere(center, r2, idx, null, null) {
  override def hasLeft: Boolean = false

  override def hasRight: Boolean = false
}

private case class Sphere(center: Vector[_3D], r2: Double)

private object Sphere {

  def fromPoints(points: Seq[Point[_3D]]): IndexedSeq[Sphere] = {
    val centers = points.map(_.toVector)
    val r2s = IndexedSeq.fill(centers.size)(1.0e-6)
    centers.zip(r2s).map(a => new Sphere(a._1, a._2)).toIndexedSeq
  }

  def fromLines(mesh: TriangleMesh3D): IndexedSeq[Sphere] = {
    val centers = mesh.pointSet.points.toIndexedSeq
    val r2s = IndexedSeq.fill(centers.size)(1.0e-6)
    centers.zip(r2s).map(a => new Sphere(a._1.toVector, a._2))
  }

  def fromTriangles(triangleList: Seq[Triangle]): IndexedSeq[Sphere] = {
    val spheres = triangleList.map { t =>
      BoundingSpheresHelper.triangleCircumSphere(t.a, t.b, t.c)
    }
    spheres.map(a => new Sphere(a._1, a._2)).toIndexedSeq
  }
}


private object BoundingSpheresHelper {

  def triangleCircumSphere(a: Vector[_3D], b: Vector[_3D], c: Vector[_3D]): (Vector[_3D], Double) = {
    // rather complex function taken from c++ ... TODO: should be checked if we cant reach the result more easily, pay attention to possible numerical problems
    var center = a
    var radius2 = 1.0

    val ab = b - a
    val ac = c - a
    val bc = c - b

    val aMb = (a + b) * 0.5
    val aMc = (a + c) * 0.5


    // handle degenerated cases
    if (ab.norm2 < Double.MinPositiveValue) {
      center = aMc
      radius2 = max((aMc - a).norm2, (aMc - c).norm2)
    } else if (ac.norm2 < Double.MinPositiveValue || bc.norm2 < Double.MinPositiveValue) {
      center = aMb
      radius2 = max((aMb - a).norm2, (aMb - b).norm2)
    } else {
      // non degenerated case

      val triangleNormal = ab.crossproduct(ac)
      val normalToAB = triangleNormal.crossproduct(ab)
      val normalToAC = triangleNormal.crossproduct(ac)

      val m = aMb - aMc

      {
        val d0 = normalToAC(0) * normalToAB(1) - normalToAC(1) * normalToAB(0)
        val d1 = normalToAC(0) * normalToAB(2) - normalToAC(2) * normalToAB(0)
        val d2 = normalToAC(1) * normalToAB(2) - normalToAC(2) * normalToAB(1)
        val beta1 = if ((abs(d0) >= abs(d1)) && (abs(d0) >= abs(d2)))
          (m(0) * normalToAB(1) - m(1) * normalToAB(0)) / d0
        else if ((abs(d1) >= abs(d0)) && (abs(d1) >= abs(d2)))
          (m(0) * normalToAB(2) - m(2) * normalToAB(0)) / d1
        else // if ((abs(d2) >= abs(d0)) && (abs(d2) >= abs(d1)))
          (m(1) * normalToAB(2) - m(2) * normalToAB(1)) / d2

        center = aMc + normalToAC * beta1

        //        {
        //          // alpha1 should lead to the same center not checked here...
        //          val alpha = if ((abs(normalToAB(0)) >= abs(normalToAB(1))) && (abs(normalToAB(0)) >= abs(normalToAB(2))))
        //            (m(0) - beta1 * normalToAC(0)) / normalToAB(0);
        //          else if ((abs(normalToAB(1)) >= abs(normalToAB(0))) && (abs(normalToAB(1)) >= abs(normalToAB(2))))
        //            (m(1) - beta1 * normalToAC(1)) / normalToAB(1);
        //          else //if ((std::abs(n1(2)) >= std::abs(n1(0))) && (std::abs(n1(2)) >= std::abs(n1(1))))
        //            (m(2) - beta1 * normalToAC(2)) / normalToAB(2);
        //
        //          val ncenter = aMb - normalToAB * alpha;
        //          if ((center - ncenter).norm2 > 1.0e-10)
        //            println("Passt nicht, sollte gleich sein" + center + " != " + ncenter)
        //        }
      }

      {
        // check weather calculated center is in the triangle...
        val d0 = ab(0) * ac(1) - ab(1) * ac(0)
        val d1 = ab(0) * ac(2) - ab(2) * ac(0)
        val d2 = ab(1) * ac(2) - ab(2) * ac(1)

        val p = center - a
        val beta2 = if ((abs(d0) >= abs(d1)) && (abs(d0) >= abs(d2)))
          (p(1) * ab(0) - p(0) * ab(1)) / d0
        else if ((abs(d1) >= abs(d0)) && (abs(d1) >= abs(d2)))
          (p(2) * ab(0) - p(0) * ab(2)) / d1
        else //if ((abs(d2) >= abs(d0)) && (abs(d2) >= abs(d1)))
          (p(2) * ab(1) - p(1) * ab(2)) / d2

        val alpha2 = if ((abs(ab(0)) >= abs(ab(1))) && (abs(ab(0)) >= abs(ab(2))))
          (p(0) - beta2 * ac(0)) / ab(0)
        else if ((abs(ab(1)) >= abs(ab(0))) && (abs(ab(1)) >= abs(ab(1))))
          (p(1) - beta2 * ac(1)) / ab(1)
        else //if ((abs(ab(2)) >= abs(ab(0))) and (abs(ab(2)) >= abs(ab(2))))
          (p(2) - beta2 * ac(2)) / ab(2)


        if (alpha2 < 0 || beta2 < 0 || alpha2 + beta2 > 1) {
          // center is outside triangle

          val r1 = ab.norm2
          val r2 = ac.norm2
          val r3 = bc.norm2
          if (r1 > r2) {
            if (r1 > r3) {
              //  radius_sqr = r1 * 0.25;
              center = aMb
            }
            else {
              //  radius_sqr = r3 * 0.25;
              center = (b + c) * 0.5
            }
          }
          else // r2 >= r1
          {
            if (r2 > r3) {
              //  radius_sqr = r2 * 0.25;
              center = aMc
            }
            else {
              //  radius_sqr = r3 * 0.25;
              center = (b + c) * 0.5
            }
          }
          // While it would be faster to use the appropriate r_i * 0.25, this is more stable.
          radius2 = max((center - a).norm2,
            (center - b).norm2,
            (center - c).norm2)
        }
        else {
          // center is in the triangle

          radius2 = max((center - a).norm2,
            (center - b).norm2,
            (center - c).norm2)
        }
      }

    }

    (center, radius2)
  }
}