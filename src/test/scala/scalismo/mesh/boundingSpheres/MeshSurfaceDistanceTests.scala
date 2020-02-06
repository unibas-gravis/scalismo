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

import breeze.linalg.{max, min}
import scalismo.ScalismoTestSuite
import scalismo.common.{PointId, UnstructuredPointsDomain}
import scalismo.geometry.{_3D, EuclideanVector, Point, Point3D}
import scalismo.mesh.{
  BarycentricCoordinates4,
  TetrahedralCell,
  TetrahedralList,
  TetrahedralMesh3D,
  TriangleCell,
  TriangleList,
  TriangleMesh3D
}
import scalismo.utils.Random

class MeshSurfaceDistanceTests extends ScalismoTestSuite {

  implicit val rnd: Random = Random(42)

  def gen(offset: Double = 0.0, scale: Double = 1.0): Double = rnd.scalaRandom.nextDouble() * scale + offset

  def randomPoint(offset: Double = 0.0, scale: Double = 1.0): Point[_3D] = {
    Point(gen(offset, scale), gen(offset, scale), gen(offset, scale))
  }

  def randomVector(offset: Double = 0.0, scale: Double = 1.0): EuclideanVector[_3D] = {
    EuclideanVector(gen(offset, scale), gen(offset, scale), gen(offset, scale))
  }

  private def randomTriangle(offset: Double, scale: Double): Triangle = {
    val a = randomVector(offset, scale)
    val b = randomVector(offset, scale)
    val c = randomVector(offset, scale)
    Triangle(a, b, c)
  }

  private def randomTetrahedron(offset: Double, scale: Double): Tetrahedron = {
    val a = randomVector(offset, scale)
    val b = randomVector(offset, scale)
    val c = randomVector(offset, scale)
    val d = randomVector(offset, scale)
    if (BoundingSphereHelpers.calculateSignedVolume(a, b, c, d) > 0)
      Tetrahedron(a, b, c, d)
    else
      Tetrahedron(a, b, d, c)
  }

  def uniform(min: Double = 50.0, max: Double = 50.0): Double = { rnd.scalaRandom.nextDouble() * (max - min) + min }

  def createCoLinearTriangle(): Triangle = {
    val b = EuclideanVector(uniform(), uniform(), uniform())
    val m = EuclideanVector(uniform(), uniform(), uniform())
    Triangle(b + m * uniform(), b + m * uniform(), b + m * uniform())
  }

  def createSinglePointTriangle(): Triangle = {
    val b = EuclideanVector(uniform(), uniform(), uniform())
    Triangle(b, b, b)
  }

  def aeqV[D](a: EuclideanVector[D], b: EuclideanVector[D], theta: Double = 1.0e-8): Boolean = {
    a.toArray.zip(b.toArray).forall(p => aeq(p._1, p._2, theta))
  }

  def aeqP[D](a: Point[D], b: Point[D], theta: Double = 1.0e-8): Boolean = {
    a.toArray.zip(b.toArray).forall(p => aeq(p._1, p._2, theta))
  }

  def aeq(a: Double, b: Double, theta: Double = 1.0e-8): Boolean = {
    a - b < theta
  }

  describe("The SurfaceDistance") {

    it("should use correct barycentric coordinate for points on a line") {
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0.0, 1.0), (10.0, 10.0), (100.0, 100.0), (-10.0, 10.0))
        pairs.foreach { pair =>
          val a = randomVector(pair._1, pair._2)
          val b = randomVector(pair._1, pair._2)

          (0 until 100) foreach { _ =>
            val s = gen()

            val p = a + (b - a) * s
            val res = BSDistance.toLineSegment(p, a, b)
            if ((b - a).norm2 < Double.MinPositiveValue) {
              res.bc._1 shouldBe 0.5 +- 1.0e-8
              res.pt shouldBe (a + b) * 0.5
            } else {
              res.bc._1 shouldBe s +- 1.0e-8
              if (res.bc._1 > 0.0) {
                if (res.bc._1 < 1.0) {
                  res.ptType shouldBe SurfaceClosestPointType.ON_LINE
                  aeqV(res.pt, p) shouldBe true
                } else {
                  res.ptType shouldBe SurfaceClosestPointType.POINT
                  res.idx._1 shouldBe 1
                  aeqV(res.pt, b) shouldBe true
                }
              } else {
                res.ptType shouldBe SurfaceClosestPointType.POINT
                res.idx._1 shouldBe 0
                aeqV(res.pt, a) shouldBe true
              }
            }
          }
        }
      }
    }

    it("should use correct barycentric coordinate for points away from the line") {
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0.0, 1.0), (10.0, 10.0), (100.0, 100.0), (-10.0, 10.0))
        pairs.foreach { pair =>
          val a = randomVector(pair._1, pair._2)
          val b = randomVector(pair._1, pair._2)

          (0 until 100) foreach { _ =>
            val s = gen()
            val ab = b - a
            val k = ab + randomVector()
            val n = ab.crossproduct(k)
            val p1 = a + ab * s
            val p = p1 + n * gen(pair._1, pair._2)
            val res = BSDistance.toLineSegment(p, a, b)
            if ((b - a).norm2 < Double.MinPositiveValue) {
              res.bc._1 shouldBe 0.5 +- 1.0e-8
              res.pt shouldBe (a + b) * 0.5
            } else {
              res.bc._1 shouldBe s +- 1.0e-8
              if (res.bc._1 > 0.0) {
                if (res.bc._1 < 1.0) {
                  res.ptType shouldBe SurfaceClosestPointType.ON_LINE
                  aeqV(res.pt, p1) shouldBe true
                } else {
                  res.ptType shouldBe SurfaceClosestPointType.POINT
                  res.idx._1 shouldBe 1
                  aeqV(res.pt, b) shouldBe true
                }
              } else {
                res.ptType shouldBe SurfaceClosestPointType.POINT
                res.idx._1 shouldBe 0
                aeqV(res.pt, a) shouldBe true
              }
            }
          }
        }
      }
    }

    it("should return the correct barycentric coordinates in a triangle") {
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0, 1), (10, 10), (100, 100), (-10, 10))
        pairs.foreach { pair =>
          val tri = randomTriangle(pair._1, pair._2)
          (0 until 100) foreach { _ =>
            val s = gen()
            val t = (1.0 - s) * gen()

            val pt = tri.a + tri.ab * s + tri.ac * t
            val p = pt + tri.n * gen(pair._1, pair._2)

            val ct = BSDistance.toTriangle(p, tri)
            val resT = ct.bc
            max(0.0, min(1.0, s)) shouldBe resT._1 +- 1.0e-8
            max(0.0, min(1.0, t)) shouldBe resT._2 +- 1.0e-8
            pt(0) shouldBe ct.pt(0) +- 1.0e-8
            pt(1) shouldBe ct.pt(1) +- 1.0e-8
            pt(2) shouldBe ct.pt(2) +- 1.0e-8

          }
        }
      }
    }

    it("should return the correct barycentric coordinates in a tetrahedron") {
      val _EPSILON = 1.0e-8
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0, 1), (10, 10), (100, 100), (-10, 10))
        pairs.foreach { pair =>
          val tet = randomTetrahedron(pair._1, pair._2)
          (0 until 100) foreach { _ =>
            val bc = BarycentricCoordinates4.randomUniform

            val pt = tet.a * bc.a + tet.b * bc.b + tet.c * bc.c + tet.d * bc.d
            val ct = BSDistance.toTetrahedron(pt, tet)
            val resT = ct.bc
            max(0.0, min(1.0, bc.b)) shouldBe resT._1 +- _EPSILON
            max(0.0, min(1.0, bc.c)) shouldBe resT._2 +- _EPSILON
            max(0.0, min(1.0, bc.d)) shouldBe resT._3 +- _EPSILON
            max(0.0, min(1.0, bc.a)) shouldBe (1.0 - resT._1 - resT._2 - resT._3) +- _EPSILON
            pt(0) shouldBe ct.pt(0) +- _EPSILON
            pt(1) shouldBe ct.pt(1) +- _EPSILON
            pt(2) shouldBe ct.pt(2) +- _EPSILON

          }
        }
      }
    }

    it("should use correct barycentric coordinates for points in triangle plane ") {
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0.0, 1.0), (10.0, 10.0), (100.0, 100.0), (-10.0, 10.0))
        pairs.foreach { pair =>
          val tri = randomTriangle(pair._1, pair._2)
          (0 until 100) foreach { _ =>
            val s = gen()
            val t = gen()

            val p = tri.a + tri.ab * s + tri.ac * t

            val res = BSDistance.calculateBarycentricCoordinates(tri, p)
            s shouldBe res._1 +- 1.0e-8
            t shouldBe res._2 +- 1.0e-8
          }
        }
      }
    }

    it("should use correct barycentric coordinates for points outside the triangle plane") {
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0, 1), (10, 10), (100, 100), (-10, 10))
        pairs.foreach { pair =>
          val tri = randomTriangle(pair._1, pair._2)
          (0 until 100) foreach { _ =>
            val s = gen()
            val t = gen()

            val p = tri.a + tri.ab * s + tri.ac * t + tri.n * gen(pair._1, pair._2)

            val res = BSDistance.calculateBarycentricCoordinates(tri, p)
            s shouldBe res._1 +- 1.0e-8
            t shouldBe res._2 +- 1.0e-8

          }
        }
      }
    }

    it("should use correct barycentric coordinates for points randomly placed near or in the tetrahedron") {
      val _EPSILON = 1.0e-8
      (0 until 100) foreach { _ =>
        val pairs = IndexedSeq((0, 1), (10, 10), (100, 100), (-10, 10))
        pairs.foreach { pair =>
          val tet = randomTetrahedron(pair._1, pair._2)
          (0 until 100) foreach { _ =>
            val pOff = randomVector(pair._1 * 2, pair._2 * 2)
            val bc = // reference implementation tested against VTK
              BarycentricCoordinates4.pointInTetrahedron(pOff.toPoint,
                                                         tet.a.toPoint,
                                                         tet.b.toPoint,
                                                         tet.c.toPoint,
                                                         tet.d.toPoint)

            val res = BSDistance.calculateBarycentricCoordinates(tet, pOff)
            bc.b shouldBe res._1 +- 1.0e-8
            bc.c shouldBe res._2 +- 1.0e-8
            bc.d shouldBe res._3 +- 1.0e-8
            bc.a shouldBe (1.0 - res._1 - res._2 - res._3) +- 1.0e-8

          }
        }
      }
    }

    it("should use reasonable barycentric coordinates for triangles with three times the same point") {

      def test(tri: Triangle): Unit = {
        for (x <- BigDecimal(0) to BigDecimal(2) by BigDecimal(0.1)) {
          val p = EuclideanVector(x.toDouble, 0, 1)
          val bc = BSDistance.calculateBarycentricCoordinates(tri, p)
          (bc._1 + bc._2 + bc._3) shouldBe 1.0 +- 1.0e-8
          val epsilon = 1.0e-12
          bc._1 should be(1.0 +- epsilon)
          bc._2 should be(0.0 +- epsilon)
          bc._3 should be(0.0 +- epsilon)
        }
      }

      for (_ <- 0 until 20) { test(createSinglePointTriangle()) }
    }

    it("should use reasonable barycentric coordinates for triangles with only co-linear points") {

      def test(tri: Triangle, pt: EuclideanVector[_3D]) = {
        val bc = BSDistance.calculateBarycentricCoordinates(tri, pt)
        (bc._1 + bc._2 + bc._3) shouldBe 1.0 +- 1.0e-8
        val epsilon = 1.0e-12
        bc._1 should be >= 0.0 - epsilon
        bc._1 should be <= 1.0 + epsilon
        bc._2 should be >= 0.0 - epsilon
        bc._2 should be <= 1.0 + epsilon
        bc._3 should be >= 0.0 - epsilon
        bc._3 should be <= 1.0 + epsilon
      }

      for (_ <- 0 until 40) {
        val tri = createCoLinearTriangle()
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }

      {
        val tri =
          Triangle(EuclideanVector(0.0, 0.0, 1.0), EuclideanVector(1.0, 0.0, 1.0), EuclideanVector(2.0, 0.0, 1.0))
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }

      {

        val tri =
          Triangle(EuclideanVector(0.0, 0.0, 1.0), EuclideanVector(2.0, 0.0, 1.0), EuclideanVector(1.0, 0.0, 1.0))
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }
      {

        val tri =
          Triangle(EuclideanVector(1.0, 0.0, 1.0), EuclideanVector(0.0, 0.0, 1.0), EuclideanVector(2.0, 0.0, 1.0))
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }
      {

        val tri =
          Triangle(EuclideanVector(1.0, 0.0, 1.0), EuclideanVector(2.0, 0.0, 1.0), EuclideanVector(0.0, 0.0, 1.0))
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }
      {

        val tri =
          Triangle(EuclideanVector(2.0, 0.0, 1.0), EuclideanVector(0.0, 0.0, 1.0), EuclideanVector(1.0, 0.0, 1.0))
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }
      {

        val tri =
          Triangle(EuclideanVector(2.0, 0.0, 1.0), EuclideanVector(1.0, 0.0, 1.0), EuclideanVector(0.0, 0.0, 1.0))
        test(tri, tri.a)
        test(tri, tri.b)
        test(tri, tri.c)
      }
    }

    it("should return the same when used for points as the findClosestPoint from UnstructuredPointsDomain") {

      val points = for (_ <- 0 until 10000) yield randomPoint()
      val pd = UnstructuredPointsDomain(points)

      val md = DiscreteSpatialIndex.fromPointList(points)

      (0 until 100) foreach { _ =>
        val p = randomPoint()

        val vpt = pd.findClosestPoint(p)
        val vd = (vpt.point - p).norm2

        val cp = md.closestPoint(p)

        vd shouldBe cp.distanceSquared
        vpt.point shouldBe cp.point
      }

    }

    it(
      "should return an equal or smaller distance when used for points than the findClosestPoint from UnstructuredPointsDomain for triangles"
    ) {

      val triangles = (0 until 100) map { _ =>
        // test if two function lead to same cp
        val a = randomVector()
        val b = randomVector()
        val c = randomVector()
        Triangle(a, b, c)
      }

      val points = triangles.flatMap(t => Array(t.a.toPoint, t.b.toPoint, t.c.toPoint))

      val pd = UnstructuredPointsDomain(points)

      val sd = TriangleMesh3DSpatialIndex.fromTriangleMesh3D(
        TriangleMesh3D(
          triangles.flatMap(t => Seq(t.a.toPoint, t.b.toPoint, t.c.toPoint)),
          TriangleList(
            (0 until 3 * triangles.length)
              .grouped(3)
              .map(g => TriangleCell(PointId(g(0)), PointId(g(1)), PointId(g(2))))
              .toIndexedSeq
          )
        )
      )

      (0 until 1000) foreach { _ =>
        val p = randomVector()

        // findClosestPoint from UnstructuredPointsDomain
        val vp = pd.findClosestPoint(p.toPoint)
        val vd = (vp.point - p.toPoint).norm2

        val ge = sd.getClosestPoint(p.toPoint)

        require(vd >= ge.distanceSquared)
      }
    }

    it("should return the same closest point on surface result when processing points in parallel") {

      val triangles = (0 until 100) map { _ =>
        // test if two function lead to same cp
        val a = randomVector()
        val b = randomVector()
        val c = randomVector()
        Triangle(a, b, c)
      }

      val sd = TriangleMesh3DSpatialIndex.fromTriangleMesh3D(
        TriangleMesh3D(
          triangles.flatMap(t => Seq(t.a.toPoint, t.b.toPoint, t.c.toPoint)),
          TriangleList(
            (0 until 3 * triangles.length)
              .grouped(3)
              .map(g => TriangleCell(PointId(g(0)), PointId(g(1)), PointId(g(2))))
              .toIndexedSeq
          )
        )
      )

      val queries = (0 until 100000) map { _ =>
        randomVector()
      }

      val cpsSeq = queries.map(q => sd.getClosestPoint(q.toPoint))
      val cpsPar = queries.par.map(q => sd.getClosestPoint(q.toPoint))

      cpsSeq.zip(cpsPar) foreach { pair =>
        val seq = pair._1
        val par = pair._2
        require(seq.point == par.point)
        require(seq.distanceSquared == par.distanceSquared)
      }

    }

    it("should return the same closest point result when processing points in parallel") {

      val triangles = (0 until 100) map { _ =>
        // test if two function lead to same cp
        val a = randomVector()
        val b = randomVector()
        val c = randomVector()
        Triangle(a, b, c)
      }

      val sd = DiscreteSpatialIndex.fromMesh(
        TriangleMesh3D(
          triangles.flatMap(t => Seq(t.a.toPoint, t.b.toPoint, t.c.toPoint)),
          TriangleList(
            (0 until 3 * triangles.length)
              .grouped(3)
              .map(g => TriangleCell(PointId(g(0)), PointId(g(1)), PointId(g(2))))
              .toIndexedSeq
          )
        )
      )

      val queries = (0 until 100000) map { _ =>
        randomVector()
      }

      val cpsSeq = queries.map(q => sd.closestPoint(q.toPoint))
      val cpsPar = queries.par.map(q => sd.closestPoint(q.toPoint))

      cpsSeq.zip(cpsPar) foreach { pair =>
        val seq = pair._1
        val par = pair._2
        require(seq.point == par.point)
        require(seq.pid == par.pid)
        require(seq.distanceSquared == par.distanceSquared)
      }
    }

    it("should create correct bounding spheres with values for center and radius which do not contain NaN.") {

      def test(tri: Triangle) = {
        val sphere = Sphere.fromTriangle(tri)
        sphere.r2.isNaN shouldBe false
        sphere.center.x.isNaN shouldBe false
        sphere.center.y.isNaN shouldBe false
        sphere.center.z.isNaN shouldBe false
      }

      for (_ <- 0 until 40) { test(createCoLinearTriangle()) }
      for (_ <- 0 until 40) { test(createSinglePointTriangle()) }
    }
  }

  describe("The BoundingSphere") {

    it("should find the correct closest points pairs in a sorted list") {

      def bruteForcePairFinder(sortedPoints: IndexedSeq[(EuclideanVector[_3D], Int)]) = {
        sortedPoints.zipWithIndex.map { e =>
          val spIndex = e._2
          val basePoint = e._1._1
          var bestIndex = (spIndex + 1) % sortedPoints.length
          var d = (basePoint - sortedPoints(bestIndex)._1).norm2
          sortedPoints.indices foreach { j =>
            val runningPoint = sortedPoints(j)._1
            val t = (basePoint - runningPoint).norm2
            if (t < d && j != spIndex) {
              d = t
              bestIndex = j
            }
          }
          (d, bestIndex, e)
        }
      }

      val centers = (0 until 10000) map { _ =>
        randomVector()
      }
      val list = centers.sortBy(a => a(1)).zipWithIndex

      val matches = BoundingSpheres.findClosestPointPairs(list)
      val testMatches = bruteForcePairFinder(list)
      matches.zip(testMatches).foreach { res =>
        val m = res._1
        val t = res._2
        m._1 shouldBe t._1
        m._2 shouldBe t._2
        m._3._2 shouldBe t._3._2
        m._3._1._2 shouldBe t._3._1._2
      }
    }

  }

  describe("The VolumeDistance") {
    it("should return not a larger value than when querying all triangles or 0 if it is inside a tetrahedron") {
      val pts = IndexedSeq(
        Point3D(0, 0, 0),
        Point3D(5, 0, 0),
        Point3D(0, 0, 5),
        Point3D(0, 5, 0),
        Point3D(5, 5, 5),
        Point3D(5, 5, 0)
      )
      val tmesh = TetrahedralMesh3D(
        pts,
        TetrahedralList(
          IndexedSeq(
            TetrahedralCell(PointId(0), PointId(2), PointId(1), PointId(4)),
            TetrahedralCell(PointId(0), PointId(3), PointId(2), PointId(4)),
            TetrahedralCell(PointId(0), PointId(1), PointId(3), PointId(4)),
            TetrahedralCell(PointId(1), PointId(5), PointId(3), PointId(4))
          )
        )
      )

      val index = TetrahedralMesh3DSpatialIndex.fromTetrahedralMesh3D(tmesh)

      def genPoint() =
        Point3D(rnd.scalaRandom.nextDouble() * 10 - 5,
                rnd.scalaRandom.nextDouble() * 10 - 5,
                rnd.scalaRandom.nextDouble() * 10 - 5)

      def isInside(pt: Point[_3D]) = {
        tmesh.tetrahedrons.exists { cell =>
          val bc = tmesh.getBarycentricCoordinates(pt, cell)
          println()
          bc.forall(d => d >= 0.0) &&
          bc.forall(d => d <= 1.0) &&
          bc.sum <= 1.0 + 1.0e-8
        }
      }

      def closestPointToOneOfTheTriangles(pt: Point[_3D]) = {
        val p = pt.toVector
        tmesh.tetrahedrons
          .flatMap { tc =>
            tc.triangles.map { t =>
              val tr = Triangle(
                tmesh.pointSet.point(t.ptId1).toVector,
                tmesh.pointSet.point(t.ptId2).toVector,
                tmesh.pointSet.point(t.ptId3).toVector
              )
              BSDistance.toTriangle(p, tr)
            }
          }
          .minBy(_.distance2)
      }

      for (i <- 0 until 200) {
        val query = genPoint()
        val cp = index.getClosestPointToVolume(query)

        if (isInside(query)) { // inside of one tetrahedron → cp should be equal query
          assert(cp.isInstanceOf[ClosestPointInTetrahedron])
          (cp.asInstanceOf[ClosestPointInTetrahedron].point - query).norm should be < 1.0e-8
        } else { // outside of a tetrahedron → cp should be the same as the minimal distance given all triangles individually
          val posCP = cp.point.toVector
          val posCPT = closestPointToOneOfTheTriangles(query).pt
          (posCP - posCPT).norm should be < 1.0e-8
        }
      }

    }

    it("should allow to reconstruct the closest point based on the meta information") {
      val pts = IndexedSeq(
        Point3D(0, 0, 0),
        Point3D(5, 0, 0),
        Point3D(0, 0, 5),
        Point3D(0, 5, 0),
        Point3D(5, 5, 5),
        Point3D(5, 5, 0)
      )
      val tmesh = TetrahedralMesh3D(
        pts,
        TetrahedralList(
          IndexedSeq(
            TetrahedralCell(PointId(0), PointId(2), PointId(1), PointId(4)),
            TetrahedralCell(PointId(0), PointId(3), PointId(2), PointId(4)),
            TetrahedralCell(PointId(0), PointId(1), PointId(3), PointId(4)),
            TetrahedralCell(PointId(1), PointId(5), PointId(3), PointId(4))
          )
        )
      )

      val index = TetrahedralMesh3DSpatialIndex.fromTetrahedralMesh3D(tmesh)

      def genPoint() =
        Point3D(rnd.scalaRandom.nextDouble() * 10 - 5,
                rnd.scalaRandom.nextDouble() * 10 - 5,
                rnd.scalaRandom.nextDouble() * 10 - 5)

      for (i <- 0 until 200) {
        val query = genPoint()
        val cp = index.getClosestPointToVolume(query)
        cp match {
          case cpiv: ClosestPointIsVertex =>
            val reconstructed = tmesh.pointSet.point(cpiv.pid)
            (cp.point - reconstructed).norm < 1.0e-8
          case cpol: ClosestPointOnLine =>
            val reconstructed = (
              tmesh.pointSet.point(cpol.pids._1).toVector * cpol.bc +
                tmesh.pointSet.point(cpol.pids._2).toVector * (1.0 - cpol.bc)
            ).toPoint
            (cp.point - reconstructed).norm < 1.0e-8
          case cpitot: ClosestPointInTriangleOfTetrahedron =>
            val tet = tmesh.tetrahedralization.tetrahedron(cpitot.tetId)
            val tri = tet.triangles(cpitot.triId.id)
            val reconstructed = (
              tmesh.pointSet.point(tri.ptId1).toVector * cpitot.bc.a +
                tmesh.pointSet.point(tri.ptId2).toVector * cpitot.bc.b +
                tmesh.pointSet.point(tri.ptId3).toVector * cpitot.bc.c
            ).toPoint
            (cp.point - reconstructed).norm < 1.0e-8
          case cpit: ClosestPointInTetrahedron =>
            val tet = tmesh.tetrahedralization.tetrahedron(cpit.tid)
            val reconstructed = (
              tmesh.pointSet.point(tet.ptId1).toVector * cpit.bc.a +
                tmesh.pointSet.point(tet.ptId2).toVector * cpit.bc.b +
                tmesh.pointSet.point(tet.ptId3).toVector * cpit.bc.c +
                tmesh.pointSet.point(tet.ptId4).toVector * cpit.bc.d
            ).toPoint
            (cp.point - reconstructed).norm < 1.0e-8
        }
      }
    }
  }
}
