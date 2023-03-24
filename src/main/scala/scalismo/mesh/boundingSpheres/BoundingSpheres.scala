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

import breeze.linalg.max
import breeze.numerics.{abs, pow, sqrt}
import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.mesh.{TetrahedralMesh, TriangleMesh3D}

import scala.annotation.tailrec

/**
 * The idea is that we wrap all elementes in hierarchical spheres as we usually can handle queries to a sphere very
 * easily. The idea for the bounding spheres is taken from the following paper of D. Maier, J. Hesser, R. Männer: Fast
 * and Accurate Closest Point Search on Triangulated Surfaces and its Application to Head Motion Estimation
 */
/**
 * Bounding sphere node of the tree structure.
 *
 * @param center
 *   Center of bounding sphere.
 * @param r2
 *   Squared radius of bounding sphere.
 * @param idx
 *   Index of entity used to form leave.
 */
abstract private[mesh] class BoundingSphere(val center: EuclideanVector[_3D],
                                            val r2: Double,
                                            val idx: Int,
                                            val left: BoundingSphere,
                                            val right: BoundingSphere
) {

  /**
   * true if left child sphere exists
   */
  def hasLeft: Boolean

  /**
   * true if right child sphere exists
   */
  def hasRight: Boolean
}

/**
 * Factory for the BoundingSphere search structure.
 */
private[mesh] object BoundingSpheres {

  /**
   * Creates a list of triangles with precalculated values.
   */
  def triangleListFromTriangleMesh3D(mesh: TriangleMesh3D): Seq[Triangle] = {

    // build triangle list (use only Vector[_3D], no Points)
    val triangles = mesh.triangulation.triangles.map { t =>
      val a = mesh.pointSet.point(t.ptId1).toVector
      val b = mesh.pointSet.point(t.ptId2).toVector
      val c = mesh.pointSet.point(t.ptId3).toVector

      Triangle(a, b, c)
    }
    triangles
  }

  def tetrahedronListFromTetrahedralMesh3D(mesh: TetrahedralMesh[_3D]): Seq[Tetrahedron] = {

    // build tetrahedron list (use only Vector[_3D], no Points)
    val tetrahedrons = mesh.tetrahedralization.tetrahedrons.map { t =>
      val a = mesh.pointSet.point(t.ptId1).toVector
      val b = mesh.pointSet.point(t.ptId2).toVector
      val c = mesh.pointSet.point(t.ptId3).toVector
      val d = mesh.pointSet.point(t.ptId4).toVector

      Tetrahedron(a, b, c, d)
    }
    tetrahedrons
  }

  /**
   * Create search index from list of points.
   */
  def createForPoints(pointList: Seq[Point[_3D]]): BoundingSphere = {
    val spheres = pointList.map(p => Sphere.fromPoint(p))
    val root = buildSearchIndex(spheres)
    root
  }

  /**
   * Create search index from list of triangles.
   */
  def createForTriangles(triangleList: Seq[Triangle]): BoundingSphere = {
    val leaves = triangleList.map(t => Sphere.fromTriangle(t))
    val root = buildSearchIndex(leaves)
    root
  }

  /**
   * Create search index from list of tetrahedrons.
   */
  def createForTetrahedrons(tetrahedronList: Seq[Tetrahedron]): BoundingSphere = {
    val leaves = tetrahedronList.map(t => Sphere.fromTetrahedron(t))
    val root = buildSearchIndex(leaves)
    root
  }

  /**
   * build search index for spheres
   */
  def buildSearchIndex(spheres: Seq[Sphere]): BoundingSphere = {
    val binarySphereLeaves = spheres.zipWithIndex.map(l => new BoundingSphereLeave(l._1.center, l._1.r2, l._2))
    buildTree(binarySphereLeaves)
  }

  /**
   * build search index recursively
   */
  final def buildTree(partitions: Seq[BoundingSphere]): BoundingSphere = {
    partitions.length match {
      case 1 =>
        partitions.head

      case _ =>
        val centers = partitions.map(s => s.center)
        val nearestPointIndex = calculateNearestPointPairs(centers)
        val subtrees = mergeNearestSpherePairs(partitions, nearestPointIndex)
        buildTree(subtrees)
    }
  }

  /**
   * merge partitions according to nearestSphereIndex
   */
  def mergeNearestSpherePairs(partitions: Seq[BoundingSphere], nearestSphereIndex: Seq[Int]): Seq[BoundingSphere] = {
    val used = Array.fill[Boolean](nearestSphereIndex.length)(false)
    val mergedTrees = nearestSphereIndex.zipWithIndex.map { p =>
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

  /**
   * merge two bounding spheres
   */
  def createSubtree(a: BoundingSphere, b: BoundingSphere): BoundingSphere = {
    val ab = b.center - a.center
    val dist2 = ab.norm2

    val (nc, nr) = if (dist2 < Double.MinPositiveValue) {
      // both have same center
      (a.center, max(a.r2, b.r2) + Double.MinPositiveValue)
    } else {
      // both have different center
      // calculate oposite points of spheres
      val na = a.center - ab * sqrt(a.r2 / dist2)
      val nb = b.center + ab * sqrt(b.r2 / dist2)
      val newCenter = (na + nb) * 0.5
      // val newRadius = ((na - nb) / 2).norm2 // @note: this is numerically unstable
      val newRadius = pow(max((newCenter - a.center).norm + sqrt(a.r2), // numerically more stable
                              (newCenter - b.center).norm + sqrt(b.r2)
                          ),
                          2
      )
      (newCenter, newRadius)
    }
    new BoundingSphereSplit(nc, nr, -1, a, b)
  }

  /**
   * calculate index of nearest points pairs
   */
  def calculateNearestPointPairs(points: Seq[EuclideanVector[_3D]]): Seq[Int] = {
    val matchedPoints = Array.fill[Int](points.length)(-1)
    val pointsWithIndex = points.zipWithIndex

    matchPoints(pointsWithIndex, matchedPoints)
    matchedPoints.toIndexedSeq
  }

  /**
   * match points recursively to get n/2 pairs and an optional single point
   */
  @tailrec
  final def matchPoints(points: Seq[(EuclideanVector[_3D], Int)], matchedPoints: Array[Int]): Unit = {
    points.length match {

      case 0 =>
      case 1 =>
        val p = points.head
        matchedPoints(p._2) = p._2

      case _ =>
        val sortedPoints = points.sortBy(_._1(1))
        val closestPointPairs = findClosestPointPairs(sortedPoints)

        val chosen: Array[Boolean] =
          choosePointPairsAndUpdateMatchedIndex(closestPointPairs, sortedPoints, matchedPoints)
        val stillActive = chosen.zipWithIndex.filter(s => !s._1).map(t => sortedPoints(t._2))
        matchPoints(stillActive.toIndexedSeq, matchedPoints)
    }
  }

  /**
   * Find best point pairs, some points might not be matched
   */
  @inline
  def choosePointPairsAndUpdateMatchedIndex(closestPointPairs: Seq[(Double, Int, ((EuclideanVector[_3D], Int), Int))],
                                            sortedPoints: Seq[(EuclideanVector[_3D], Int)],
                                            matchedPoints: Array[Int]
  ): Array[Boolean] = {
    val chosen = Array.fill[Boolean](closestPointPairs.length)(false)
    val bestPairs = closestPointPairs.sortBy(a => a._1)
    bestPairs.foreach { cp =>
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

  /**
   * Find for each point the closest neighbour
   */
  @inline
  def findClosestPointPairs(sortedPoints: Seq[(EuclideanVector[_3D], Int)]) = {
    sortedPoints.zipWithIndex.map { e =>
      val spIndex = e._2
      val basePoint = e._1._1

      var bestIndex = (spIndex + 1) % sortedPoints.length
      var d = (basePoint - sortedPoints(bestIndex)._1).norm2
      ((spIndex + 2) until sortedPoints.length).takeWhile { j =>
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

      ((spIndex - 1) to 0 by -1).takeWhile { j =>
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

/**
 * Inner node of the search index.
 */
private class BoundingSphereSplit(center: EuclideanVector[_3D],
                                  r2: Double,
                                  idx: Int,
                                  left: BoundingSphere,
                                  right: BoundingSphere
) extends BoundingSphere(center, r2, idx, left, right) {
  override def hasLeft: Boolean = left != null

  override def hasRight: Boolean = right != null
}

/**
 * Leave node of the search index.
 */
private class BoundingSphereLeave(center: EuclideanVector[_3D], r2: Double, idx: Int)
    extends BoundingSphere(center, r2, idx, null, null) {

  override def hasLeft: Boolean = false

  override def hasRight: Boolean = false
}

/**
 * Helper class to build BoundingSphereLeaves
 */
private case class Sphere(center: EuclideanVector[_3D], r2: Double)

/**
 * Factory for Sphere class.
 */
private object Sphere {

  import BoundingSphereHelpers._

  /**
   * Create spheres around points with radius.
   */
  def fromPoint(point: Point[_3D], radius: Double = 1.0e-6): Sphere = {
    Sphere(point.toVector, radius)
  }

  /**
   * Create spheres around a line.
   */
  def fromLine(line: (EuclideanVector[_3D], EuclideanVector[_3D])): Sphere = {
    val (center, r2) = minContainmentSphere(line._1, line._2)
    Sphere(center, r2)
  }

  /**
   * Create sphere around a triangle
   */
  def fromTriangle(triangle: Triangle): Sphere = {
    val (center, r2) = minContainmentSphere(triangle.a, triangle.b, triangle.c)
    Sphere(center, r2)
  }

  /**
   * Create sphere around a triangle
   */
  def fromTetrahedron(tetrahedron: Tetrahedron): Sphere = {
    val (center, r2) = minContainmentSphere(tetrahedron.a, tetrahedron.b, tetrahedron.c, tetrahedron.d)
    Sphere(center, r2)
  }

}

private[mesh] object BoundingSphereHelpers {

  /**
   * Calculate minimal sphere around two points, e.g. a line segment
   */
  def minContainmentSphere(a: EuclideanVector[_3D], b: EuclideanVector[_3D]): (EuclideanVector[_3D], Double) = {
    val center = (a + b) * 0.5
    val r2 = max((a - center).norm2, (b - center).norm2)
    (center, r2)
  }

  /**
   * Calculate minimal sphere around three points, e.g. a triangle
   */
  def minContainmentSphere(a: EuclideanVector[_3D],
                           b: EuclideanVector[_3D],
                           c: EuclideanVector[_3D]
  ): (EuclideanVector[_3D], Double) = {
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
      radius2 = max((center - a).norm2, (center - c).norm2)
    } else if (ac.norm2 < Double.MinPositiveValue || bc.norm2 < Double.MinPositiveValue) {
      center = aMb
      radius2 = max((center - a).norm2, (center - b).norm2)
    } else if (abs(ab.normalize.dot(ac.normalize)) == 1.0) {
      // all points on same line
      val lengths = Seq(("ab", ab.norm), ("ac", ac.norm), ("bc", bc.norm))
      lengths.maxBy(_._2)._1 match {
        case "ab" =>
          center = aMb
          radius2 = max((center - a).norm2, (center - b).norm2)
        case "ac" =>
          center = aMc
          radius2 = max((center - a).norm2, (center - c).norm2)
        case "bc" =>
          center = (b + c) * 0.5
          radius2 = max((center - b).norm2, (center - c).norm2)
      }
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
        val beta1 =
          if ((abs(d0) >= abs(d1)) && (abs(d0) >= abs(d2)))
            (m(0) * normalToAB(1) - m(1) * normalToAB(0)) / d0
          else if ((abs(d1) >= abs(d0)) && (abs(d1) >= abs(d2)))
            (m(0) * normalToAB(2) - m(2) * normalToAB(0)) / d1
          else // if ((abs(d2) >= abs(d0)) && (abs(d2) >= abs(d1)))
            (m(1) * normalToAB(2) - m(2) * normalToAB(1)) / d2

        center = aMc + normalToAC * beta1
      }

      {
        // check weather calculated center is in the triangle...
        val d0 = ab(0) * ac(1) - ab(1) * ac(0)
        val d1 = ab(0) * ac(2) - ab(2) * ac(0)
        val d2 = ab(1) * ac(2) - ab(2) * ac(1)

        val p = center - a
        val beta2 =
          if ((abs(d0) >= abs(d1)) && (abs(d0) >= abs(d2)))
            (p(1) * ab(0) - p(0) * ab(1)) / d0
          else if ((abs(d1) >= abs(d0)) && (abs(d1) >= abs(d2)))
            (p(2) * ab(0) - p(0) * ab(2)) / d1
          else // if ((abs(d2) >= abs(d0)) && (abs(d2) >= abs(d1)))
            (p(2) * ab(1) - p(1) * ab(2)) / d2

        val alpha2 =
          if ((abs(ab(0)) >= abs(ab(1))) && (abs(ab(0)) >= abs(ab(2))))
            (p(0) - beta2 * ac(0)) / ab(0)
          else if ((abs(ab(1)) >= abs(ab(0))) && (abs(ab(1)) >= abs(ab(1))))
            (p(1) - beta2 * ac(1)) / ab(1)
          else // if ((abs(ab(2)) >= abs(ab(0))) and (abs(ab(2)) >= abs(ab(2))))
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
            } else {
              //  radius_sqr = r3 * 0.25;
              center = (b + c) * 0.5
            }
          } else // r2 >= r1
            {
              if (r2 > r3) {
                //  radius_sqr = r2 * 0.25;
                center = aMc
              } else {
                //  radius_sqr = r3 * 0.25;
                center = (b + c) * 0.5
              }
            }
          // While it would be faster to use the appropriate r_i * 0.25, this is more stable.
          radius2 = max((center - a).norm2, (center - b).norm2, (center - c).norm2)
        } else {
          // center is in the triangle

          radius2 = max((center - a).norm2, (center - b).norm2, (center - c).norm2)
        }
      }

    }

    (center, radius2)
  }

  /**
   * Calculate sphere around four points, e.g. a tetrahedron
   */
  def minContainmentSphere(a: EuclideanVector[_3D],
                           b: EuclideanVector[_3D],
                           c: EuclideanVector[_3D],
                           d: EuclideanVector[_3D]
  ): (EuclideanVector[_3D], Double) = {

    val triangles = IndexedSeq(
      IndexedSeq(a, b, c),
      IndexedSeq(a, d, b),
      IndexedSeq(a, c, d),
      IndexedSeq(b, d, c)
    )

    def testOrientation(circumCenter: EuclideanVector[_3D],
                        a: EuclideanVector[_3D],
                        b: EuclideanVector[_3D],
                        c: EuclideanVector[_3D],
                        d: EuclideanVector[_3D]
    ): Seq[Double] = {
      val signedTetrahedronVolume = calculateSignedVolume(a, b, c, d)
      triangles
        .map { t =>
          calculateSignedVolume(circumCenter, t(0), t(1), t(2))
        }
        .map(_ * Math.signum(signedTetrahedronVolume))
    }

    val tetrahedronCircumsphere = calculateCircumsphere(a, b, c, d)
    val directionTests = testOrientation(tetrahedronCircumsphere._1, a, b, c, d)

    directionTests.count(_ <= 0) match {
      case 4 => { // circum-center is inside the tetrahedron, no better possibility
        tetrahedronCircumsphere
      }
      case 3 => { // circum-center is outside / on the wrong side of one triangle, use its circum-sphere
        val t = triangles(directionTests.indexWhere(_ > 0))
        minContainmentSphere(t(0), t(1), t(2))
      }
      case 2 => { // circum-center is outside / on the wrong side of two triangles
        val i1 = directionTests.indexWhere(_ > 0)
        val points1 = triangles(i1)
        val i2 = directionTests.indexWhere(_ > 0, i1 + 1)
        val points2 = triangles(i2)

        val commonPoints = points1.filter(pt => {
          points2.contains(pt)
        })

        val sphereContainingLine = minContainmentSphere(commonPoints(0), commonPoints(1))

        val pts =
          IndexedSeq(a, b, c, d).filter { p =>
            !sphereContainsPoint(p, sphereContainingLine._1, Math.sqrt(sphereContainingLine._2))
          }

        if (pts.size > 0) {
          minContainmentSphere(commonPoints.head, commonPoints.last, pts.head)
        } else {
          sphereContainingLine
        }
      }
      case _ =>
        throw new Exception(
          "It should never be the case that more orientations are negative than 2."
        )
    }
  }

  /**
   * Calculate circumsphere, i.e. the sphere which touches all four points.
   */
  def calculateCircumsphere(
    a: EuclideanVector[_3D],
    b: EuclideanVector[_3D],
    c: EuclideanVector[_3D],
    d: EuclideanVector[_3D]
  ): (EuclideanVector[_3D], Double) = {

    val t = a - d
    val u = b - d
    val v = c - d

    val q = u.crossproduct(v) * t.norm2 + v.crossproduct(t) * u.norm2 + t
      .crossproduct(u) * v.norm2

    val det2 = 2.0 * determinantVectorsInRows(t, u, v)
    val center = d + q / det2
    val radius = (q / det2).norm2
    (center, radius)
  }

  /**
   * Checks weather all points lie within the sphere described by the center and the radius.
   */
  def sphereContainsPoints(points: IndexedSeq[EuclideanVector[_3D]], center: EuclideanVector[_3D], radius: Double) = {
    points.forall(pt => sphereContainsPoint(pt, center, radius))
  }

  /**
   * Checks weather the point lies within the sphere described by the center and the radius.
   */
  def sphereContainsPoint(point: EuclideanVector[_3D], center: EuclideanVector[_3D], radius: Double) = {
    val dist = (point - center).norm
    dist - radius < 1e-8
  }

  /**
   * Calculates the signed volume of the tetrahedron, i.e. if all normals point in- or out-wards
   */
  def calculateSignedVolume(a: EuclideanVector[_3D],
                            b: EuclideanVector[_3D],
                            c: EuclideanVector[_3D],
                            d: EuclideanVector[_3D]
  ): Double = {
    val t = b - a
    val u = c - a
    val v = d - a
    determinantVectorsInRows(t, u, v)
  }

  /**
   * Helper function to calculate determinant of the matrix when vectors are stacked into the rows.
   */
  def determinantVectorsInRows(t: EuclideanVector[_3D], u: EuclideanVector[_3D], v: EuclideanVector[_3D]): Double =
    (t.x * u.y * v.z + u.x * v.y * t.z + v.x * t.y * u.z - t.x * v.y * u.z - v.x * u.y * t.z - u.x * t.y * v.z)

}
