package smptk.kernels

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.geometry.Point1D
import smptk.geometry.implicits._

class KernelTransformationTests extends FunSpec with ShouldMatchers {

  describe("a Kernel") {
    it("yields correct multiple when  multiplied by a scalar") {
      val gk = GaussianKernel1D(3.5)
      val gkMult = gk * 100
      val pt1 = 0.1
      val pt2 = 1.0
      gk(pt1, pt2) * 100.0 should be(gkMult(pt1, pt2))
    }

    it("yields correct result when two kernels are added") {
      val gk = GaussianKernel1D(3.5)
      val gk2 = gk + gk 
      val pt1 = 0.1
      val pt2 = 1.0
      gk(pt1, pt2) + gk(pt1, pt2) should be(gk2(pt1, pt2))

    }

  }
  describe("A scalar valued Gaussian kernel") {
    it("evaluated with twice the same argument yields 1") {
      val gk = GaussianKernel1D(3.5)
      gk(0.1, 0.1)(0, 0) should be(1.0 plusOrMinus 1e-8)
    }

    it("given two arguments far apart yields almost 0") {
      val gk = GaussianKernel1D(1.0)
      gk(0.1,100)(0, 0) should be(0.0 plusOrMinus 1e-8)
    }
  }
}