package org.statismo.stk.core.kernels

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry.implicits._
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.geometry.{Point, ThreeD}
import org.statismo.stk.core.statisticalmodel.{LowRankGaussianProcess, LowRankGaussianProcessConfiguration}
import org.statismo.stk.core.geometry.Vector3D
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.geometry.Point3D
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.numerics.UniformDistributionRandomSampler3D
import org.statismo.stk.core.geometry

class KernelTests extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

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
      gk(0.1, 0.1) should be(1.0 plusOrMinus 1e-8)
    }

    it("given two arguments far apart yields almost 0") {
      val gk = GaussianKernel1D(1.0)
      gk(0.1, 100) should be(0.0 plusOrMinus 1e-8)
    }
  }

  describe("A sample covariance kernel") {
    it("can reproduce the covariance function from random samples") {


      val domain = BoxedDomain3D(Point3D(-5, 1, 3), Point3D(100, 90, 25))
      val samplerForNystromApprox = UniformSampler3D(domain, 7 * 7 * 7)



      val k = UncorrelatedKernel3x3(GaussianKernel3D(100.0))
      val mu = (pt: Point[ThreeD]) => Vector3D(1, 10, -5)
      val gpConf = LowRankGaussianProcessConfiguration(domain, samplerForNystromApprox, mu, k, 500)

      val gp = LowRankGaussianProcess.createLowRankGaussianProcess3D(gpConf)


      val sampleTransformations = for (i <- (0 until 5000).par) yield {
        // TODO: gp.sample() should (arguably) accept seed.
        val sample: (Point[ThreeD] => geometry.Vector[ThreeD]) = gp.sample
        new Transformation[ThreeD] {
          def apply(x: Point[ThreeD]) = x + sample(x)

          def takeDerivative(x: Point[ThreeD]) = ???
        }
      }


      val testPtSampler = UniformDistributionRandomSampler3D(domain, 1)
      val pts = testPtSampler.sample.map(_._1)

      val sampleCovKernel = SampleCovarianceKernel3D(sampleTransformations.toIndexedSeq, pts.size)

      // since mu always returns the same vector, it's enough to calculate it once
      val mux = mu(Point3D(0, 0, 0))
      for (x <- pts.par) {
        val mu2 = sampleCovKernel.mu(x)
        for (d <- 0 until 3) {
          mu2(d) should be(mux(d) plusOrMinus 0.2f)
        }
      }

      for (x <- pts.par; y <- pts) {
        val gpxy = gp.cov(x, y)
        val sampleCovxy = sampleCovKernel(x, y)
        for (d1 <- 0 until 3; d2 <- 0 until 3) {
          sampleCovxy(d1, d2) should be(gpxy(d1, d2) plusOrMinus 0.2f)
        }
      }
    }
  }

}