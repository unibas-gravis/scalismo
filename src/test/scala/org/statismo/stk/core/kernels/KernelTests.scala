package org.statismo.stk.core.kernels

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry.Point1D
import org.statismo.stk.core.geometry.implicits._
import org.statismo.stk.core.io.StatismoIO
import java.io.File
import org.statismo.stk.core.registration.KernelTransformationSpaceConfiguration
import org.statismo.stk.core.registration.KernelTransformationSpace3D
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.geometry.{ Point, ThreeD, MatrixNxN }
import scala.util.Random
import breeze.stats.distributions.RandBasis
import org.statismo.stk.core.statisticalmodel.SpecializedLowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.GaussianProcess
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcessConfiguration
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcessConfiguration
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.numerics.Sampler
import scala.compat.Platform
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcessConfiguration
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcess3D
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcessConfiguration
import org.statismo.stk.core.geometry.Vector3D
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.geometry.Point3D
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.numerics.UniformDistributionRandomSampler3D
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.utils.Visualization._
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel

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

      val domain = BoxedDomain3D(Point3D(-5, 1, 3), Point3D(200, 180, 50))

      val nystromSampler = UniformSampler3D(domain, 7 * 7 * 7)

      val k = UncorrelatedKernel3x3(GaussianKernel3D(100.0))
      val mu = (pt: Point[ThreeD]) => Vector3D(1, 10, -5)
      val gpConf = LowRankGaussianProcessConfiguration(domain, nystromSampler, mu, k, 200)
      val gp = GaussianProcess.createLowRankGaussianProcess3D(gpConf)

      val sampleTransformations = for (i <- (0 until 500).par) yield {
        val sample = gp.sample
        new Transformation[ThreeD] {
          def apply(x: Point[ThreeD]) = x + sample(x)
          def takeDerivative(x: Point[ThreeD]) = ???
        }
      }


      val testPtSampler = UniformDistributionRandomSampler3D(domain, 100)
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