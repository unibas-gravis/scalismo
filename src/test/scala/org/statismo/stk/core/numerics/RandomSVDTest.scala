package org.statismo.stk.core.numerics

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.kernels._

class RandomSVDTest extends FunSpec with ShouldMatchers {


  describe("The random svd") {

    it("accurately approximates the first 10 eigenvectors and eigenvalues of a gaussian kernel matrix") {
<<<<<<< HEAD
      val k = UncorrelatedKernel1x1(GaussianKernel1D(10))
      val xs = (0 until 500).map(x => Point1D(x))

=======
      val k = UncorrelatedKernel1x1(GaussianKernel1D(100))
      val xs = (0 until 1000).map(x => Point1D(x))
>>>>>>> release-0.5
      val K = Kernel.computeKernelMatrix(xs, k)
      val Kdouble = K.map(_.toDouble)
      val (ur, lr, vrt) = RandomSVD.computeSVD(Kdouble, 10)
      val (ub, lb, vbt) = breeze.linalg.svd(Kdouble)


      val mr = ur(::, 0 until 10) * breeze.linalg.diag(lr(0 until 10)) * vrt(0 until 10, 0 until 10)
      val mb = ub(::, 0 until 10) * breeze.linalg.diag(lb(0 until 10)) * vbt(0 until 10, 0 until 10)

      for (j <- 0 until 10; i <- 0 until mr.rows) {
        val factor = Math.max(1e-5, mr(i, j)) / Math.max(1e-5, mb(i, j))
        factor should be(1.0 plusOrMinus 0.01)
      }
    }
  }
}