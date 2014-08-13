package org.statismo.stk.core.numerics

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.geometry._

class RandomSVDTest extends FunSpec with ShouldMatchers {


  describe("The random svd") {
    it("accurately approximates the first 10 eigenvectors and eigenvalues of a gaussian kernel matrix") {
      val k = UncorrelatedKernel1x1(GaussianKernel1D(10))
      val xs = (0 until 500).map(x => Point1D(x))

      val K = Kernel.computeKernelMatrix(xs, k)
      val Kdouble = K.map(_.toDouble)
      val (ur, lr, vrt) = RandomSVD.computeSVD(Kdouble, 10)
      val (ub, lb, vbt) = breeze.linalg.svd(Kdouble)


      for (j <- 0 until 10) {
        (lr(j) should be(lb(j) plusOrMinus 0.005))

        for (i <- (0 until ur.rows)) {
          (math.abs(ur(i, j)) should be(math.abs(ub(i, j)) plusOrMinus 0.005))
        }

        for (i <- (0 until vrt.rows)) {
          (math.abs(vrt(i, j)) should be(math.abs(vbt(i, j)) plusOrMinus 0.005))
        }

      }
    }
  }
}