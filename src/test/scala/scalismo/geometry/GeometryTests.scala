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
package scalismo.geometry

import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.ScalismoTestSuite
import scalismo.registration._
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.utils.Random

import scala.language.implicitConversions

class GeometryTests extends ScalismoTestSuite {

  implicit val random = Random(42)

  val p = Point(0.1, 3.0, 1.1)
  val pGeneric: Point[_3D] = p
  val v = Vector(0.1, 3.0, 1.1)
  val vGeneric: Vector[_3D] = v

  def checkPoint[D <: Dim: NDSpace]() = {
    def randomPoint(): Point[D] = Point[D](Array.fill(NDSpace[D].dimensionality)(random.scalaRandom.nextDouble()))
    val pt = randomPoint()

    describe(s"A random nD Point $pt (n=${NDSpace[D].dimensionality})") {
      it("equals a Point with identical values") {
        pt should equal(pt.map(v => v))
      }

      it("fulfills p + (p2 - p) - (p2 - p) == p") {
        val p2 = randomPoint()
        pt + (p2 - pt) - (p2 - pt) should equal(pt)
      }

      it("converts to an Array and back") {
        Point[D](pt.toArray) should equal(pt)
      }

      it("converts to a Vector and back") {
        pt.toVector.toPoint should be(pt)
      }

      it("converts to an Array of proper length") {
        pt.toArray should have length NDSpace[D].dimensionality
      }

      it("converts to a Breeze vector and back") {
        Point.fromBreezeVector[D](pt.toBreezeVector) should equal(pt)
      }

      it("can map a constant value p.map(f) == 0 (f: x => 0.0)") {
        val z = Point[D](Array.fill(NDSpace[D].dimensionality)(0.0))
        def f(x: Double): Double = 0.0
        pt.map(f) should equal(z)
      }

      it("can map a function p.map(f).map(g) == 0 (f: x => 2.0+x, g: x => x-2.0)") {
        val z = Point[D](Array.fill(NDSpace[D].dimensionality)(0))
        val f = (x: Double) => 2.0 + x
        val g = (x: Double) => x - 2.0
        (pt.map(f).map(g) - pt).norm should be < 1e-4
      }

      it("can map a function using its index: p.mapWithIndex(f) == 0 (f: (v,i) => v - p(i))") {
        val z = Point[D](Array.fill(NDSpace[D].dimensionality)(0.0))
        pt.mapWithIndex((v, i) => v - pt(i)) should equal(z)
      }
    }
  }

  // test all dimensions for Point
  checkPoint[_1D]()
  checkPoint[_2D]()
  checkPoint[_3D]()

  def checkVector[D <: Dim: NDSpace]() = {
    def randomVector(): Vector[D] = Vector[D](Array.fill(NDSpace[D].dimensionality)(random.scalaRandom.nextDouble()))
    val v = randomVector()

    describe(s"A random nD Vector $v (n=${NDSpace[D].dimensionality})") {

      it("equals a Vector with identical values") {
        v should equal(v.map(f => f))
      }

      it("fulfills v*(-1) + v*(1) == 0") {
        v * (-1) + v * 1 should equal(Vector.zeros[D])
      }

      it("fulfills v - v == 0") {
        v - v should equal(Vector.zeros[D])
      }

      it("converts to an Array and back") {
        Vector[D](v.toArray) should equal(v)
      }

      it("converts to an Array of proper length") {
        v.toArray should have length NDSpace[D].dimensionality
      }

      it("converts to a Breeze vector and back") {
        Vector.fromBreezeVector[D](v.toBreezeVector) should equal(v)
      }

      it("can map a constant value v.map(f) == 0 (f: x => 0.0)") {
        val z = Vector[D](Array.fill(NDSpace[D].dimensionality)(0.0))
        def f(x: Double): Double = 0.0
        v.map(f) should equal(z)
      }

      it("can map a function v.map(f).map(g) == 0 (f: x => 2.0+x, g: x => x-2.0)") {
        val z = Vector[D](Array.fill(NDSpace[D].dimensionality)(0.0))
        val f = (x: Double) => 2.0 + x
        val g = (x: Double) => x - 2.0
        (v.map(f).map(g) - v).norm should be < 1e-4
      }

      it("can map a function using its index: v.mapWithIndex(f) == 0 (f: (x,i) => x - v(i))") {
        val z = Vector[D](Array.fill(NDSpace[D].dimensionality)(0.0))
        v.mapWithIndex((x, i) => x - v(i)) should equal(z)
      }

      it("converts to a Vector and back") {
        v.toPoint.toVector should be(v)
      }

      it("inner product probably (1 example) fulfills dot(v,v) >= 0") {
        v.dot(v) should be >= 0.0
      }

      it("inner product probably (1 example) fulfills dot(a*v,w) == a*dot(v,w)") {
        val a = random.scalaRandom.nextDouble()
        val w = randomVector()
        (v * a).dot(w) - a * v.dot(w) should be < 1.0e-4
      }

      it("inner product probably (1 example) fulfills dot(v+a,w) == dot(v,w)+dot(a,w)") {
        val w = randomVector()
        val a = randomVector()
        (v + a).dot(w) - (v.dot(w) + a.dot(w)) should be < 1.0e-4
      }

      it("inner product is probably (1 example) symmetric") {
        val w = randomVector()
        v.dot(w) - w.dot(v) should be < 1.0e-4
      }

      it("has the proper relation between norm and norm2") {
        math.sqrt(v.norm2) should be(v.norm +- 1e-5)
      }

      it("probably (1 example) has a positive norm") {
        v.norm should be > 0.0
      }

      it("provides a zero norm for zero vectors") {
        Vector.zeros[D].norm should be(0.0 +- 1e-10)
      }

      it("has norm which probably (1 example) fulfills the triangle equality") {
        val w = randomVector()
        (v + w).norm should be <= (v.norm + w.norm + 1e-6)
      }

      it("has norm which probably (1 example) fulfills (a*v).norm == |a|*v.norm(v)") {
        val a = random.scalaRandom.nextDouble()
        (v * a).norm - math.abs(a) * v.norm should be <= 1e-6
      }

      it("has a norm which is derived from the inner product: dot(v,v)==v.norm2") {
        v.norm2 should be(v.dot(v))
      }

      val f = math.max(1e-10, random.scalaRandom.nextDouble())
      it(s"fulfills norm((v*f)/f - v) ~ 0 (f=$f)") {
        ((v * f) / f - v).norm should be(0.0 +- 1e-4)
      }
    }
  }

  // test all dimensions for Vector
  checkVector[_1D]()
  checkVector[_2D]()
  checkVector[_3D]()

  def checkIndex[D <: Dim: NDSpace]() = {
    def randomIndex(): IntVector[D] = IntVector[D](Array.fill(NDSpace[D].dimensionality)(random.scalaRandom.nextInt()))
    val ind = randomIndex()

    describe(s"A random nD Index $ind (n=${NDSpace[D].dimensionality})") {
      it("equals a Index with identical values") {
        ind should equal(ind.map(v => v))
      }

      it("converts to an Array and back") {
        IntVector[D](ind.toArray) should equal(ind)
      }

      it("converts to an Array of proper length") {
        ind.toArray should have length NDSpace[D].dimensionality
      }

      it("converts to a Breeze vector and back") {
        IntVector[D](ind.toBreezeVector.data) should equal(ind)
      }

      it("can map a constant value p.map(f) == 0 (f: x => 0)") {
        val z = IntVector[D](Array.fill(NDSpace[D].dimensionality)(0))
        def f(x: Int): Int = 0
        ind.map(f) should equal(z)
      }

      it("can map a function p.map(f).map(g) == 0 (f: x => 2 + x, g: x => x - 2)") {
        val z = IntVector[D](Array.fill(NDSpace[D].dimensionality)(0))
        val f = (x: Int) => 2 + x
        val g = (x: Int) => x - 2
        ind.map(f).map(g) should equal(ind)
      }

      it("can map a function using its index: p.mapWithIndex(f) == 0 (f: (v,i) => v - p(i))") {
        val z = IntVector[D](Array.fill(NDSpace[D].dimensionality)(0))
        ind.mapWithIndex((v, i) => v - ind(i)) should equal(z)
      }
    }
  }

  // test all dimensions for Index
  checkIndex[_1D]()
  checkIndex[_2D]()
  checkIndex[_3D]()

  describe("A 3D Point") {
    it("equals a Point[ThreeD]") {
      p should equal(pGeneric)
    }

    it("does not equal a Vector[ThreeD]") {
      p should not equal v
      p should not equal vGeneric
    }
    it("can be described as a Vector3D") {
      p.toVector should equal(v)
    }

    it("can be mapped with a function f: (Double) => Double") {
      val p = Point(6.0, 3.0, 3.0)
      val pHalf = Point(3.0, 1.5, 1.5)
      p should equal(pHalf.map(f => f * 2))
    }

    it("can be mapped with Index a function f: (Double,Int) => Double") {
      val p = Point(1.0, 1.0, 1.0)
      val pRes = Point(0.0, 1.0, 2.0)
      pRes should equal(p.mapWithIndex({ case (v, i) => i * v }))
    }

  }

  describe("A 3D Vector") {
    it("equals a Vector[ThreeD]") {
      v should equal(vGeneric)
    }

    it("does not equal a Point[ThreeD]") {
      v should not equal p
      v should not equal pGeneric
    }

    it("equals a point when converted to point") {
      v.toPoint should equal(p)
    }

    it("gives the correct norm and normsquared for various test cases") {
      Vector(1.0).norm2 should equal(1)
      Vector(1.0, 1.0).norm2 should equal(2)
      Vector(1.0, 1.0, 1.0).norm2 should equal(3)
      Vector(math.sqrt(2), math.sqrt(2)).norm2 should be(4.0 +- 1e-5)
      v.norm should be(math.sqrt(v.norm2) +- 1e-5)
    }

    it("gives the correct dot value for the dot product") {
      val v1 = Vector(4.9, -3.5, -1.0)
      val v2 = Vector(3.1, 2.1, 5.0)
      v1 dot v2 should be(v1.toBreezeVector dot v2.toBreezeVector)
    }

    it("gives the correct value for the outer product") {
      val v1 = Vector(4.0, -3.0, -1.0)
      val v2 = Vector(3.0, 2.0, 5.0)
      val res = SquareMatrix((12.0, 8.0, 20.0), (-9.0, -6.0, -15.0), (-3.0, -2.0, -5.0))
      (v1 outer v2) should be(res)
    }

    it("gives the correct value for the cross product") {
      val v1 = Vector(4.0, -3.0, -1.0)
      val v2 = Vector(3.0, 2.0, 5.0)
      val crossPdBreeze = breeze.linalg.cross(v1.toBreezeVector, v2.toBreezeVector)
      v1.crossproduct(v2) should be(Vector(crossPdBreeze(0), crossPdBreeze(1), crossPdBreeze(2)))
    }

    it("can be mapped with a function f: (Double) => Double") {
      val v = Point(6.0, 3.0, 3.0)
      val vHalf = Point(3.0, 1.5, 1.5)
      v should equal(vHalf.map(f => f * 2))
    }

    it("can be mapped with Index a function f: (Double,Int) => Double") {
      val v = Point(1.0, 1.0, 1.0)
      val vRes = Point(0.0, 1.0, 2.0)
      vRes should equal(v.mapWithIndex({ case (f, i) => f * i }))
    }
  }

  describe("Polar and Spherical coordinates constructors") {
    it("A 3D point can be constructed from spherical coordinates (with random values)") {
      Point.fromSpherical(2.0, 2.0, 0.5) shouldBe Point(1.595967130708011, 0.8718808172146366, -0.8322936730942848)
    }

    it("A 2D point can be constructed from polar coordinates (with random values)") {
      Point.fromPolar(2.0, 0.5) shouldBe Point(1.7551651237807455, 0.958851077208406)
    }

    it("polar and spherical coordinates are consistent in xy plane") {
      val p2d: Point2D = Point.fromPolar(1.5, 0.2)
      val p3d: Point3D = Point.fromSpherical(1.5, math.Pi / 2, 0.2)
      (Point(p2d.x, p2d.y, 0.0) - p3d).norm should be < 1e-4
    }

    it("A 3D vector constructed from spherical coordinates is identical to the corresponding point") {
      Vector.fromSpherical(3.0, 3.0, 5.0) shouldBe Point.fromSpherical(3.0, 3.0, 5.0).toVector
    }

    it("A 2D vector constructed from spherical coordinates is identical to the corresponding point") {
      Vector.fromPolar(2.0, 0.5) shouldBe Point.fromPolar(2.0, 0.5).toVector
    }
  }

  describe("a 3x3 matrix") {

    // storage is column major
    val m = SquareMatrix[_3D](Array(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))

    it("can be created using zeros") {
      val m = SquareMatrix.zeros[_3D]
      for (i <- 0 until 3; j <- 0 until 3) {
        m(i, j) should be(0.0)
      }
    }

    it("can be correctly initialized by a tuple") {
      val mInitFromTuple = SquareMatrix((1.1, 1.2, 1.3), (2.1, 2.2, 2.3), (3.1, 3.2, 3.3))
      mInitFromTuple should equal(m)
    }

    it("equals another 3x3 matrix with the same values") {
      val m = SquareMatrix[_3D](Array(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))
      val m2 = SquareMatrix[_3D](Array(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))
      m should equal(m2)
    }

    it("can be converted to a breeze matrix") {
      val mbreeze = DenseMatrix.create[Double](3, 3, Array[Double](1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))
      m.toBreezeMatrix should equal(mbreeze)
    }

    it("can be indexed correctly") {
      val mbreeze = m.toBreezeMatrix
      m(1, 2) should equal(2.3)
      m(1, 2) should equal(mbreeze(1, 2))
      m(0, 0) should equal(1.1)
      m(0, 0) should equal(mbreeze(0, 0))
      m(2, 1) should equal(3.2)
      m(2, 1) should equal(mbreeze(2, 1))
      m(2, 2) should equal(3.3)
    }

    it("can be multiplied by a vector") {
      val v = Vector(1.0, 2.0, 3.0)
      val vBreeze = DenseVector(1.0, 2.0, 3.0)
      val mxv = m * v
      val mxvBreeze = m.toBreezeMatrix * vBreeze
      for (i <- 0 until 3) {
        mxv(i) should be(mxvBreeze(i) +- 1e-8)
      }
    }

    it("can be multiplied by a scalar") {
      val m1 = (m * 0.1).toBreezeMatrix
      val m2 = m.toBreezeMatrix * 0.1
      for (i <- 0 until 3; j <- 0 until 3) {
        m1(i, j) should be(m2(i, j) +- 1e-8)
      }
    }

    it("can be added to another matrix") {
      val m1 = m + m
      val m2 = m.toBreezeMatrix + m.toBreezeMatrix
      for (i <- 0 until 3; j <- 0 until 3) {
        m1(i, j) should be(m2(i, j) +- 1e-8)
      }
    }

    it("can be multiplied (elementwise) with another matrix") {
      val m1 = m :* m
      val m2 = m.toBreezeMatrix :* m.toBreezeMatrix
      for (i <- 0 until 3; j <- 0 until 3) {
        m1(i, j) should be(m2(i, j) +- 1e-8)
      }
    }

    it("can be multiplied (matrix product) with another matrix") {
      val m = SquareMatrix((1.0, 2.0, 3.0), (2.0, 7.0, 3.0), (9.0, 2.0, 8.0))
      val m2 = SquareMatrix((3.0, 4.0, 1.0), (3.0, 7.0, 2.0), (7.0, 9.0, 11.0))

      val res = m * m2
      val resBreeze = m.toBreezeMatrix * m2.toBreezeMatrix

      for (i <- 0 until 3; j <- 0 until 3) {
        res(i, j) should be(resBreeze(i, j) +- 1e-5)
      }
    }

    it("fulfills some simple identities with ones,zeros and ident") {
      val v = Vector(1.0, 2.0, 3.0)
      SquareMatrix.eye[_3D] * v should equal(v)
      SquareMatrix.zeros[_3D] * v should equal(Vector(0.0, 0.0, 0.0))
      SquareMatrix.ones[_3D] * v should equal(Vector(6.0, 6.0, 6.0))
    }

    it("yields itself when transposed twice") {
      val m = SquareMatrix((3.0, 4.0, 1.0), (3.0, 7.0, 2.0), (7.0, 9.0, 11.0))
      val mtt = m.t.t
      for (i <- 0 until 3; j <- 0 until 3) {
        mtt(i, j) should equal(m(i, j))
      }
    }

    it("yields the identity transform when inverted and multiplied with itself") {
      val m = SquareMatrix((3.0, 4.0, 1.0), (3.0, 7.0, 2.0), (7.0, 9.0, 11.0))
      val mInvertible = m.t * m // m^T *m is always invertible
      val almostEye = mInvertible * SquareMatrix.inv(mInvertible)
      val eye = SquareMatrix.eye[_3D]
      for (i <- 0 until 3; j <- 0 until 3) {
        almostEye(i, j) should be(eye(i, j) +- 1e-5)
      }
    }
  }

  describe("a 2D landmark") {

    val pcs = Seq((DenseVector(1.0, 0.0), 2.0), (DenseVector(0.0, 1.0), 0.5))
    val lm = Landmark("a", Point2D(1, 1), None, Some(MultivariateNormalDistribution(DenseVector.zeros[Double](2), pcs)))

    it("is correctly transformed using an identity transform") {

      val transformedLm = lm.transform(Transformation((p: Point[_2D]) => p))

      lm.id should equal(transformedLm.id)
      lm.description should equal(transformedLm.description)
      lm.point should equal(transformedLm.point)

      // the uncertainty is transformed stochastically. We therefore do not require strict equivalence
      breeze.linalg.norm(lm.uncertainty.get.mean - transformedLm.uncertainty.get.mean) should be < 1e-2
      breeze.linalg.sum((lm.uncertainty.get.cov - transformedLm.uncertainty.get.cov).toDenseMatrix) should be < 1e-2
    }

    it("is correctly transformed using a rigid transform") {

      val rigidTransform = RigidTransformation(TranslationTransform(Vector2D(2, 3)),
        RotationSpace[_2D]().transformForParameters(DenseVector(Math.PI / 2)))

      val transformedLm = lm.transform(rigidTransform)

      lm.id should equal(transformedLm.id)
      lm.description should equal(transformedLm.description)
      rigidTransform(lm.point) should equal(transformedLm.point)

      // the uncertainty is transformed stochastically. We therefore do not require strict equivalence
      breeze.linalg.norm(lm.uncertainty.get.mean - transformedLm.uncertainty.get.mean) should be < 1e-2

      // a rigid transformation retains the variance
      for (i <- 0 until 2) {
        lm.uncertainty.get.principalComponents(i)._2 should be(transformedLm.uncertainty.get.principalComponents(i)._2 +- 1e-1)
      }
    }


  }

}
