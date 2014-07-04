package org.statismo.stk.core.kernels

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.geometry.{ Point, _3D, MatrixNxN, Vector }
import org.statismo.stk.core.statisticalmodel.{LowRankGaussianProcess, DiscreteGaussianProcess, LowRankGaussianProcessConfiguration}
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.numerics.UniformDistributionRandomSampler3D

class KernelTransformationTests extends FunSpec with ShouldMatchers {
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

      val domain = BoxedDomain3D(Point(-5, 1, 3), Point(100, 80, 50))

      val nystromSampler = UniformSampler3D(domain, 8 * 8 * 8)

      val k = UncorrelatedKernel3x3(GaussianKernel3D(100.0))
      val mu = (pt: Point[_3D]) => Vector(1, 10, -5)
      val gpConf = LowRankGaussianProcessConfiguration(domain, nystromSampler, mu, k, 500)
      val gp = LowRankGaussianProcess.createLowRankGaussianProcess3D(gpConf)

      val sampleTransformations = for (i <- (0 until 3000).par) yield {
        val sample = gp.sample
        new Transformation[_3D] {
          def apply(x: Point[_3D]) = x + sample(x)
          def takeDerivative(x: Point[_3D]) = ???
        }
      }


      val testPtSampler = UniformDistributionRandomSampler3D(domain, 1)
      val pts = testPtSampler.sample.map(_._1)


      val sampleCovKernel = SampleCovarianceKernel3D(sampleTransformations.toIndexedSeq, pts.size)
      
      for (x <- pts.par) {
        for (d <- 0 until 3) {
          sampleCovKernel.mu(x)(d) should be(mu(x)(d) plusOrMinus (0.2f))
        }
      }

      for (x <- pts.par; y <- pts) {
        val kxy = k(x, y)
        val sampleCovxy = sampleCovKernel(x, y)
        for (d1 <- 0 until 3; d2 <- 0 until 3) {
          kxy(d1, d2) should be(sampleCovxy(d1, d2) plusOrMinus (0.2f))
        }
      }

    }
  }

}