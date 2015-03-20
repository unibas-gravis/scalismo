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
package scalismo.kernels

import org.scalatest.{ Matchers, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import scalismo.common.{ RealSpace, VectorField, BoxDomain }
import scalismo.geometry.{ _3D, _1D, Point, Vector }
import Point.implicits._
import scalismo.geometry
import scalismo.numerics.UniformSampler
import scalismo.registration.Transformation
import scalismo.statisticalmodel.{ GaussianProcess, LowRankGaussianProcess }

class KernelTests extends FunSpec with Matchers {
  scalismo.initialize()

  describe("a Kernel") {
    it("yields correct multiple when  multiplied by a scalar") {
      val gk = GaussianKernel[_1D](3.5)
      val gkMult = gk * 100
      val pt1 = 0.1f
      val pt2 = 1.0f
      gk(pt1, pt2) * 100.0 should be(gkMult(pt1, pt2))
    }

    it("yields correct result when two kernels are added") {
      val gk = GaussianKernel[_1D](3.5)
      val gk2 = gk + gk
      val pt1 = 0.1f
      val pt2 = 1.0f
      gk(pt1, pt2) + gk(pt1, pt2) should be(gk2(pt1, pt2))
    }
  }
  describe("A scalar valued Gaussian kernel") {
    it("evaluated with twice the same argument yields 1") {
      val gk = GaussianKernel[_1D](3.5)
      gk(0.1f, 0.1f) should be(1.0 +- 1e-8)
    }

    it("given two arguments far apart yields almost 0") {
      val gk = GaussianKernel[_1D](1.0)
      gk(0.1f, 100) should be(0.0 +- 1e-8)
    }
  }

  describe("A sample covariance kernel") {
    it("can reproduce the covariance function from random samples") {

      val domain = BoxDomain[_3D](Point(-5, 1, 3), Point(100, 90, 25))

      val samplerForNystromApprox = UniformSampler(domain, 7 * 7 * 7)

      val k = UncorrelatedKernel[_3D](GaussianKernel[_3D](100.0))
      val mu = (pt: Point[_3D]) => Vector(1, 10, -5)
      val gp = LowRankGaussianProcess.approximateGP(GaussianProcess(VectorField(domain, mu), k), samplerForNystromApprox, 500)

      val sampleTransformations = for (i <- (0 until 5000).par) yield {
        // TODO: gp.sample() should (arguably) accept seed.
        val sample: (Point[_3D] => geometry.Vector[_3D]) = gp.sample
        new Transformation[_3D] {
          override val domain = RealSpace[_3D]
          override val f = (x: Point[_3D]) => x + sample(x)
        }
      }

      val testPtSampler = UniformSampler(domain, 1)
      val pts = testPtSampler.sample.map(_._1)

      val sampleCovKernel = SampleCovarianceKernel[_3D](sampleTransformations.toIndexedSeq, pts.size)

      // since mu always returns the same vector, it's enough to calculate it once
      val mux = mu(Point(0, 0, 0))
      for (x <- pts.par) {
        val mu2 = sampleCovKernel.mu(x)
        for (d <- 0 until 3) {
          mu2(d) should be(mux(d) +- 0.2f)
        }
      }

      for (x <- pts.par; y <- pts) {
        val gpxy = gp.cov(x, y)
        val sampleCovxy = sampleCovKernel(x, y)
        for (d1 <- 0 until 3; d2 <- 0 until 3) {
          sampleCovxy(d1, d2) should be(gpxy(d1, d2) +- 0.2f)
        }
      }
    }
  }

  describe("Two scalar valued kernels") {
    it("can be added and multiplied") {
      val k1 = GaussianKernel[_1D](1.0)
      val k2 = GaussianKernel[_1D](1.0)
      val ksum = k1 + k2
      val x = Point(0);
      val y = Point(1);
      ksum(x, y) should be(k1(x, y) + k2(x, y) +- 1e-5)

      val kprod = k1 * k2
      kprod(x, y) should be(k1(x, y) * k2(x, y) +- 1e-5)

      // test scalar multiplication
      (k1 * 2.0)(x, y) should be(k1(x, y) * 2.0 +- 1e-5)
    }
  }

  describe("Two matrix valued kernels") {
    it("can be added and multiplied") {
      val k1 = UncorrelatedKernel[_1D](GaussianKernel[_1D](1.0))
      val k2 = UncorrelatedKernel[_1D](GaussianKernel[_1D](1.0))
      val ksum = k1 + k2
      val x = Point(0);
      val y = Point(1);
      ksum(x, y)(0, 0) should be((k1(x, y) + k2(x, y))(0, 0) +- 1e-5f)

      val kprod = k1 * k2
      kprod(x, y)(0, 0) should be((k1(x, y) * k2(x, y))(0, 0) +- 1e-5f)

      // test scalar multiplication
      (k1 * 2.0)(x, y)(0, 0) should be(k1(x, y)(0, 0) * 2.0f +- 1e-5f)
    }
  }

}