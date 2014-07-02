package org.statismo.stk.core.geometry

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.language.implicitConversions

class GeometryTests extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  val p = Point(0.1, 3.0, 1.1)
  val pGeneric: Point[ThreeD] = p
  val v = Vector(0.1, 3.0, 1.1)
  val vGeneric: Vector[ThreeD] = v

  describe("A 3D Point") {
    it("equals a Point[ThreeD]") {
      p should equal(pGeneric)
    }

    it("does not equal a Vector[ThreeD]") {
      p should not equal (v)
      p should not equal (vGeneric)
    }
    it("can be described as a Vector3D") {
      p.toVector should equal(v)
    }

  }

  describe("A 3D Vector") {
    it("equals a Vector[ThreeD]") {
      v should equal(vGeneric)
    }

    it("does not equal a Point[ThreeD]") {
      v should not equal (p)
      v should not equal (pGeneric)
    }

    it("equals a point when converted to point") {
      v.toPoint should equal(p)
    }

    it("gives the correct norm and normsquared for various test cases") {
      Vector(1).norm2 should equal(1)
      Vector(1, 1).norm2 should equal(2)
      Vector(1, 1, 1).norm2 should equal(3)
      Vector(math.sqrt(2).toFloat, math.sqrt(2).toFloat).norm2 should be(4.0 plusOrMinus (1e-5))
      v.norm should be(math.sqrt(v.norm2) plusOrMinus (1e-5))
    }

    it("gives the correct dot value for the dot product") {
      val v1 = Vector(4.9, -3.5, -1.0)
      val v2 = Vector(3.1, 2.1, 5.0)
      v1 dot v2 should be(v1.toBreezeVector dot v2.toBreezeVector)
    }

    it("gives the correct value for the outer product") {
      val v1 = Vector(4.0, -3.0, -1.0)
      val v2 = Vector(3.0, 2.0, 5.0)
      val res = MatrixNxN((12f, 8f, 20f), (-9f, -6f, -15f), (-3f, -2f, -5f))
      (v1 outer v2) should be(res)
    }

    it("gives the correct value for the cross product") {
      val v1 = Vector(4.0, -3.0, -1.0)
      val v2 = Vector(3.0, 2.0, 5.0)
      val crossPdBreeze = breeze.linalg.cross(v1.toBreezeVector, v2.toBreezeVector)
      Vector.crossproduct(v1, v2) should be(Vector3D(crossPdBreeze(0), crossPdBreeze(1), crossPdBreeze(2)))
    }

  }
  describe("a 3x3 matrix") {

    // storage is column major
    val m = MatrixNxN[ThreeD](Array(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3).map(_.toFloat))

    it("can be created using zeros") {
      val m = MatrixNxN.zeros[ThreeD]
      for (i <- 0 until 3; j <- 0 until 3) {
        m(i, j) should be(0f)
      }
    }

    it("can be correclty initialized by a tuple") {
      val mInitFromTuple = MatrixNxN((1.1, 1.2, 1.3), (2.1, 2.2, 2.3), (3.1, 3.2, 3.3))
      mInitFromTuple should equal(m)
    }

    it("should euqal another 3x3 matrix with the same values") {
      val m = MatrixNxN[ThreeD](Array(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))
      val m2 = MatrixNxN[ThreeD](Array(1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))
      m should equal(m2)
    }

    it("can be converted to a breeze matrix") {
      val mbreeze = DenseMatrix.create[Float](3, 3, Array[Float](1.1, 2.1, 3.1, 1.2, 2.2, 3.2, 1.3, 2.3, 3.3))
      m.toBreezeMatrix should equal(mbreeze)
    }

    it("can be indexed correctly") {
      val mbreeze = m.toBreezeMatrix
      m(1, 2) should equal(2.3f)
      m(1, 2) should equal(mbreeze(1, 2))
      m(0, 0) should equal(1.1f)
      m(0, 0) should equal(mbreeze(0, 0))
      m(2, 1) should equal(3.2f)
      m(2, 1) should equal(mbreeze(2, 1))
      m(2, 2) should equal(3.3f)
    }

    it("can be multiplied by a vector") {
      val v = Vector3D(1, 2, 3)
      val vBreeze = DenseVector(1f, 2f, 3f)
      val mxv = m * v
      val mxvBreeze = m.toBreezeMatrix * vBreeze
      for (i <- 0 until 3) {
        mxv(i) should be(mxvBreeze(i) plusOrMinus (1e-8))
      }

    }

    it("can be multiplied by a scalar") {
      val m1 = (m * 0.1).toBreezeMatrix
      val m2 = m.toBreezeMatrix * 0.1f
      for (i <- 0 until 3; j <- 0 until 3) {
        m1(i, j) should be(m2(i, j) plusOrMinus (1e-8))
      }
    }

    it("can be added to another matrix") {
      val m1 = m + m
      val m2 = m.toBreezeMatrix + m.toBreezeMatrix
      for (i <- 0 until 3; j <- 0 until 3) {
        m1(i, j) should be(m2(i, j) plusOrMinus (1e-8))
      }
    }

    it("can be multiplied (elementwise) with another matrix") {
      val m1 = m :* m
      val m2 = m.toBreezeMatrix :* m.toBreezeMatrix
      for (i <- 0 until 3; j <- 0 until 3) {
        m1(i, j) should be(m2(i, j) plusOrMinus (1e-8))
      }
    }

    it("can be multiplied (matrix product) with another matrix") {
      val m = MatrixNxN((1, 2, 3), (2, 7, 3), (9, 2, 8))
      val m2 = MatrixNxN((3, 4, 1), (3, 7, 2), (7, 9, 11))

      val res = m * m2
      val resBreeze = m.toBreezeMatrix * m2.toBreezeMatrix

      for (i <- 0 until 3; j <- 0 until 3) {
        res(i, j) should be(resBreeze(i, j) plusOrMinus (1e-5))
      }
    }

    it("fullfills some simple identities with ones,zeros and ident") {
      val v = Vector3D(1, 2, 3)
      MatrixNxN.eye[ThreeD] * v should equal(v)
      MatrixNxN.zeros[ThreeD] * v should equal(Vector(0, 0, 0))
      MatrixNxN.ones[ThreeD] * v should equal(Vector(6, 6, 6))
    }

    it("yields itself when transposed twise") {
      val m = MatrixNxN((3, 4, 1), (3, 7, 2), (7, 9, 11))
      val mtt = m.t.t
      for (i <- 0 until 3; j <- 0 until 3) {
        mtt(i,j) should equal (m(i,j))
      }
    }

    it("yields the identity transform when inverted and multiplied with itself") {
      val m = MatrixNxN((3, 4, 1), (3, 7, 2), (7, 9, 11))
      val mInvertible = m.t * m // m^T *m is always invertible
      val almostEye = mInvertible * MatrixNxN.inv(mInvertible)
      val eye = MatrixNxN.eye[ThreeD]
      for (i <- 0 until 3; j <- 0 until 3)  {
        almostEye(i,j) should be(eye(i,j) plusOrMinus 1e-5f)
      }
    }
  }

}
