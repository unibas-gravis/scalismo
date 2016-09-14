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

import breeze.linalg.{ max, min }
import scalismo.ScalismoTestSuite
import scalismo.common.{ PointId, UnstructuredPointsDomain }
import scalismo.geometry.{ Dim, Point, Vector, _3D }
import scalismo.mesh.{ TriangleCell, TriangleList, TriangleMesh3D }
import scalismo.utils.Random

class MeshSurfaceDistanceTests extends ScalismoTestSuite {

  implicit val rnd = Random(42)

  def rgen(offset: Double = 0.0, scale: Double = 1.0) = rnd.scalaRandom.nextDouble() * scale + offset

  def randomPoint(offset: Double = 0.0, scale: Double = 1.0)(implicit rnd: Random): Point[_3D] = {
    Point(rgen(offset, scale), rgen(offset, scale), rgen(offset, scale))
  }

  def randomVector(offset: Double = 0.0, scale: Double = 1.0)(implicit rnd: Random): Vector[_3D] = {
    Vector(rgen(offset, scale), rgen(offset, scale), rgen(offset, scale))
  }

  private def randomTriangle(offset: Double = 0.0, scale: Double = 1.0): Triangle = {
    val a = randomVector(offset, scale)
    val b = randomVector(offset, scale)
    val c = randomVector(offset, scale)
    Triangle(a, b, c, b - a, c - a, (b - a).crossproduct(c - a))
  }

  def aeqV[D <: Dim](a: Vector[D], b: Vector[D], theta: Double = 1.0e-8): Boolean = {
    a.toArray.zip(b.toArray).forall(p => aeq(p._1, p._2, theta))
  }

  def aeqP[D <: Dim](a: Point[D], b: Point[D], theta: Double = 1.0e-8): Boolean = {
    a.toArray.zip(b.toArray).forall(p => aeq(p._1, p._2, theta))
  }

  def aeq(a: Double, b: Double, theta: Double = 1.0e-8): Boolean = {
    a - b < theta
  }

  describe("The SurfaceDistance") {

    it("should use correct barycentric coordinate for points on a line") {
      (0 until 100) foreach { j =>
        val pairs = IndexedSeq((0.0, 1.0), (10.0, 10.0), (100.0, 100.0), (-10.0, 10.0))
        pairs.foreach { pair =>
          val a = randomVector(pair._1, pair._2)
          val b = randomVector(pair._1, pair._2)

          (0 until 100) foreach { i =>
            val s = rgen()

            val p = a + (b - a) * s
            val res = BSDistance.toLineSegment(p, a, b)
            if ((b - a).norm2 < Double.MinPositiveValue) {
              res.bc._1 shouldBe 0.5 +- 1.0e-8
              res.pt shouldBe (a + b) * 0.5
            } else {
              res.bc._1 shouldBe s +- 1.0e-8
              if (res.bc._1 > 0.0) {
                if (res.bc._1 < 1.0) {
                  res.ptType shouldBe ClosestPointType.ON_LINE
                  aeqV(res.pt, p) shouldBe true
                } else {
                  res.ptType shouldBe ClosestPointType.POINT
                  res.idx._1 shouldBe 1
                  aeqV(res.pt, b) shouldBe true
                }
              } else {
                res.ptType shouldBe ClosestPointType.POINT
                res.idx._1 shouldBe 0
                aeqV(res.pt, a) shouldBe true
              }
            }
          }
        }
      }
    }

    it("should use correct barycentric coordinate for points away from the line") {
      (0 until 100) foreach { j =>
        val pairs = IndexedSeq((0.0, 1.0), (10.0, 10.0), (100.0, 100.0), (-10.0, 10.0))
        pairs.foreach { pair =>
          val a = randomVector(pair._1, pair._2)
          val b = randomVector(pair._1, pair._2)

          (0 until 100) foreach { i =>
            val s = rgen()
            val ab = b - a
            val k = ab + randomVector()
            val n = ab.crossproduct(k)
            val p1 = a + ab * s
            val p = p1 + n * rgen(pair._1, pair._2)
            val res = BSDistance.toLineSegment(p, a, b)
            if ((b - a).norm2 < Double.MinPositiveValue) {
              res.bc._1 shouldBe 0.5 +- 1.0e-8
              res.pt shouldBe (a + b) * 0.5
            } else {
              res.bc._1 shouldBe s +- 1.0e-8
              if (res.bc._1 > 0.0) {
                if (res.bc._1 < 1.0) {
                  res.ptType shouldBe ClosestPointType.ON_LINE
                  aeqV(res.pt, p1) shouldBe true
                } else {
                  res.ptType shouldBe ClosestPointType.POINT
                  res.idx._1 shouldBe 1
                  aeqV(res.pt, b) shouldBe true
                }
              } else {
                res.ptType shouldBe ClosestPointType.POINT
                res.idx._1 shouldBe 0
                aeqV(res.pt, a) shouldBe true
              }
            }
          }
        }
      }
    }

    it("should return the correct barycentric coordinates in a triangle") {
      (0 until 100) foreach { j =>
        val pairs = IndexedSeq((0, 1), (10, 10), (100, 100), (-10, 10))
        pairs.foreach { pair =>
          val tri = randomTriangle(pair._1, pair._2)
          (0 until 100) foreach { i =>
            val s = rgen()
            val t = (1.0 - s) * rgen()

            val pt = tri.a + tri.ab * s + tri.ac * t
            val p = pt + tri.n * rgen(pair._1, pair._2)

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

    it("should use correct barycentric coordinates for points in triangle plane ") {
      (0 until 100) foreach { j =>
        val pairs = IndexedSeq((0.0, 1.0), (10.0, 10.0), (100.0, 100.0), (-10.0, 10.0))
        pairs.foreach { pair =>
          val tri = randomTriangle(pair._1, pair._2)
          (0 until 100) foreach { i =>
            val s = rgen()
            val t = rgen()

            val p = tri.a + tri.ab * s + tri.ac * t

            val res = BSDistance.calculateBarycentricCoordinates(tri, p)
            s shouldBe res._1 +- 1.0e-8
            t shouldBe res._2 +- 1.0e-8
          }
        }
      }
    }

    it("should use correct barycentric coordinates for points outside the triangle plane") {
      (0 until 100) foreach { j =>
        val pairs = IndexedSeq((0, 1), (10, 10), (100, 100), (-10, 10))
        pairs.foreach { pair =>
          val tri = randomTriangle(pair._1, pair._2)
          (0 until 100) foreach { i =>
            val s = rgen()
            val t = rgen()

            val p = tri.a + tri.ab * s + tri.ac * t + tri.n * rgen(pair._1, pair._2)

            val res = BSDistance.calculateBarycentricCoordinates(tri, p)
            s shouldBe res._1 +- 1.0e-8
            t shouldBe res._2 +- 1.0e-8

          }
        }
      }
    }

    it("should return the same when used for points as the findClosestPoint from UnstructuredPointsDomain") {

      val points = for (i <- 0 until 10000) yield randomPoint()
      val pd = UnstructuredPointsDomain(points)

      val md = DiscreteSpatialIndex.fromPointList(points)

      (0 until 100) foreach { i =>
        val p = randomPoint()

        val vpt = pd.findClosestPoint(p)
        val vdist = (vpt.point - p).norm2

        val cp = md.closestPoint(p)

        vdist shouldBe cp.distance2
        vpt.point shouldBe cp.point
      }

    }

    it("should return an equal or smaller distance when used for points than the findClosestPoint from UnstructuredPointsDomain for triangles") {

      val triangles = (0 until 100) map { j =>
        // test if two function lead to same cp
        val a = randomVector()
        val b = randomVector()
        val c = randomVector()
        Triangle(a, b, c, b - a, c - a, (b - a).crossproduct(c - a))
      }

      val points = triangles.flatMap(t => Array(t.a.toPoint, t.b.toPoint, t.c.toPoint))

      val pd = UnstructuredPointsDomain(points)

      val sd = SurfaceSpatialIndex.fromTriangleMesh3D(TriangleMesh3D(
        triangles.flatMap(t => Seq(t.a.toPoint, t.b.toPoint, t.c.toPoint)),
        TriangleList((0 until 3 * triangles.length).grouped(3).map(g => TriangleCell(PointId(g(0)), PointId(g(1)), PointId(g(2)))).toIndexedSeq)
      ))

      (0 until 1000) foreach { i =>
        val p = randomVector()

        // findClosestPoint from UnstructuredPointsDomain
        val vp = pd.findClosestPoint(p.toPoint)
        val vd = (vp.point - p.toPoint).norm2

        val ge = sd.getClosestPoint(p.toPoint)

        require(vd >= ge._2)
      }
    }

    it("should return the same closest point on surface result when processing points in parallel") {

      val triangles = (0 until 100) map { j =>
        // test if two function lead to same cp
        val a = randomVector()
        val b = randomVector()
        val c = randomVector()
        Triangle(a, b, c, b - a, c - a, (b - a).crossproduct(c - a))
      }

      val sd = SurfaceSpatialIndex.fromTriangleMesh3D(TriangleMesh3D(
        triangles.flatMap(t => Seq(t.a.toPoint, t.b.toPoint, t.c.toPoint)),
        TriangleList((0 until 3 * triangles.length).grouped(3).map(g => TriangleCell(PointId(g(0)), PointId(g(1)), PointId(g(2)))).toIndexedSeq)
      ))

      val queries = (0 until 100000) map { i =>
        randomVector()
      }

      val cpsSeq = queries.map(q => sd.getClosestPoint(q.toPoint))
      val cpsPar = queries.par.map(q => sd.getClosestPoint(q.toPoint))

      cpsSeq.zip(cpsPar) foreach { pair =>
        val seq = pair._1
        val par = pair._2
        require(seq._1 == par._1)
        require(seq._2 == par._2)
      }

    }

    it("should return the same closest point result when processing points in parallel") {

      val triangles = (0 until 100) map { j =>
        // test if two function lead to same cp
        val a = randomVector()
        val b = randomVector()
        val c = randomVector()
        Triangle(a, b, c, b - a, c - a, (b - a).crossproduct(c - a))
      }

      val points = triangles.flatMap(t => Array(t.a.toPoint, t.b.toPoint, t.c.toPoint))

      val sd = DiscreteSpatialIndex.fromMesh(TriangleMesh3D(
        triangles.flatMap(t => Seq(t.a.toPoint, t.b.toPoint, t.c.toPoint)),
        TriangleList((0 until 3 * triangles.length).grouped(3).map(g => TriangleCell(PointId(g(0)), PointId(g(1)), PointId(g(2)))).toIndexedSeq)
      ))

      val queries = (0 until 100000) map { i =>
        randomVector()
      }

      val cpsSeq = queries.map(q => sd.closestPoint(q.toPoint))
      val cpsPar = queries.par.map(q => sd.closestPoint(q.toPoint))

      cpsSeq.zip(cpsPar) foreach { pair =>
        val seq = pair._1
        val par = pair._2
        require(seq.point == par.point)
        require(seq.idx == par.idx)
        require(seq.distance2 == par.distance2)
      }
    }

  }

  describe("The BoundingSphere") {

    it("should find the correct closest points pairs in a sorted list") {

      def bruteForcePairFinder(sortedPoints: IndexedSeq[(Vector[_3D], Int)]) = {
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

      val centers = (0 until 10000) map { i =>
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

}
