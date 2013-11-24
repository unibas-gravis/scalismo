package org.statismo.stk.core.statisticalmodel

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.implicits._
import org.statismo.stk.core.numerics._
import org.statismo.stk.core.image.DiscreteImageDomain1D
import breeze.linalg.DenseVector
import org.statismo.stk.core.image.DiscreteImageDomain2D
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.image.DiscreteImageDomain3D
import java.io.File
import GaussianProcess._
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.common.BoxedDomain2D
import org.statismo.stk.core.common.BoxedDomain3D

class GaussianProcessTests extends FunSpec with ShouldMatchers {
  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("A Gaussian process regression") {
    it("keeps the landmark points fixed for a 1D case") {
      val domain = BoxedDomain1D(-5.0, 5)
      val kernel = UncorrelatedKernel1x1(GaussianKernel1D(5))
      val config = LowRankGaussianProcessConfiguration[OneD](domain, UniformSampler1D(domain, 500), _ => Vector1D(0f), kernel, 100)
      val gp = GaussianProcess.createLowRankGaussianProcess1D(config)

      val trainingData = IndexedSeq((-3.0, 1.0), (-1.0, 3.0), (0.0, -1.0), (1.0, -1.0), (3.0, 0.0)).map(t => (Point1D(t._1), Vector1D(t._2)))
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-8)

      for ((x, y) <- trainingData) {
        (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 1e-1))
      }
    }
  }

  it("keeps the landmark points fixed for a 2D case") {
    val domain = BoxedDomain2D((-5.0, -5.0), (5.0, 5.0))
    val config = LowRankGaussianProcessConfiguration[TwoD](domain, UniformSampler2D(domain, 400), _ => Vector2D(0.0, 0.0), UncorrelatedKernel2x2(GaussianKernel2D(5)), 100)
    val gp = GaussianProcess.createLowRankGaussianProcess2D(config)

    val trainingData = IndexedSeq((Point2D(-3.0, -3.0), Vector2D(1.0, 1.0)), (Point2D(-1.0, 3.0), Vector2D(0.0, -1.0)))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
    }
  }

  it("keeps the landmark points fixed for a 3D case") {
    val domain = BoxedDomain3D((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
    val config = LowRankGaussianProcessConfiguration[ThreeD](domain, UniformSampler3D(domain, 8 * 8 * 8), _ => Vector3D(0.0, 0.0, 0.0), UncorrelatedKernel3x3(GaussianKernel3D(5)), 100)
    val gp = GaussianProcess.createLowRankGaussianProcess3D(config)

    val trainingData = IndexedSeq((Point3D(-3.0, -3.0, -1.0), Vector3D(1.0, 1.0, 2.0)), (Point3D(-1.0, 3.0, 0.0), Vector3D(0.0, -1.0, 0.0)))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(2) should be(y(2) plusOrMinus 0.0001))
    }

  }

  describe("a lowRankGaussian process") {
    object Fixture {
      val domain = BoxedDomain3D((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = UniformSampler3D(domain, 7 * 7 * 7)
      val kernel = UncorrelatedKernel3x3(GaussianKernel3D(30) * 10)
      val gp = {
        val config = LowRankGaussianProcessConfiguration[ThreeD](domain, sampler, _ => Vector3D(0.0, 0.0, 0.0), kernel, 100)
        GaussianProcess.createLowRankGaussianProcess3D(config)
      }
    }

    it("yields the same covariance as given by the kernel") {
      val f = Fixture
      val fewPointsSampler = UniformSampler3D(f.domain, 2 * 2 * 2)
      val pts = fewPointsSampler.sample.map(_._1)
      for (pt1 <- pts.par; pt2 <- pts) {
        val covGP = f.gp.cov(pt1, pt2)
        val covKernel = f.kernel(pt1, pt2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGP(i, j) should be(covKernel(i, j) plusOrMinus 1e-2)
        }
      }
    }

//    it("approximates correctly the first eigenfunction of a given gp") {
//      val f = Fixture
//      // create a new gp with cov from the given gp
//      val config = LowRankGaussianProcessConfiguration[ThreeD](f.domain, f.sampler, _ => Vector3D(0.0, 0.0, 0.0), f.gp.cov, 2)
//      val newGp = GaussianProcess.createLowRankGaussianProcess3D(config)
//      val (gpLambdas, gpPhis) = f.gp.eigenPairs.unzip
//      val (newgpLambdas, newGpPhis) = newGp.eigenPairs.unzip
//
////      for ((gpLambda, newgplambda) <- gpLambdas.zip(newgpLambdas)) {
////        gpLambda should be (newgplambda plusOrMinus 1e-3)
////      }
//      
//      val pts = f.sampler.sample.map(_._1)
//      for (pt <- pts) {
//        val phiVal = gpPhis(1)(pt)
//        val newPhiVal = newGpPhis(1)(pt)
//        println(s"${newgpLambdas(1)} * ${newPhiVal(1
//            )} should be(${gpLambdas(1)} * ${phiVal(1)} plusOrMinus 1e-2)")
//        newgpLambdas(0) * newPhiVal(0) should be(gpLambdas(0) * phiVal(0) plusOrMinus 1e-2)
// 
//      }
//
//    }
    
  }

  describe("a specialized Gaussian process") {

    object Fixture {
      val domain = BoxedDomain3D((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = UniformSampler3D(domain, 6 * 6 * 6)
      val gp = {
        val config = LowRankGaussianProcessConfiguration[ThreeD](domain, sampler, _ => Vector3D(0.0, 0.0, 0.0), UncorrelatedKernel3x3(GaussianKernel3D(5)), 100)
        GaussianProcess.createLowRankGaussianProcess3D(config)
      }
      val specializedPoints = sampler.sample.map(_._1)
      val specializedGp = gp.specializeForPoints(specializedPoints)
    }

    it("yields the same deformations at the specialized points") {
      val f = Fixture

      val coeffs = DenseVector.zeros[Float](f.gp.eigenPairs.size)
      val gpInstance = f.gp.instance(coeffs)
      val specializedGpInstance = f.specializedGp.instance(coeffs)
      for (pt <- f.specializedPoints) {
        gpInstance(pt) should equal(specializedGpInstance(pt))
      }

      for ((pt, df) <- f.specializedGp.instanceAtPoints(coeffs)) {
        df should equal(gpInstance(pt))
      }
    }

    it("yields the same result for gp regression as a normal gp") {
      val f = Fixture

      val trainingData = IndexedSeq((Point3D(-3.0, -3.0, -1.0), Vector3D(1.0, 1.0, 2.0)), (Point3D(-1.0, 3.0, 0.0), Vector3D(0.0, -1.0, 0.0)))
      val posteriorGP = GaussianProcess.regression(f.gp, trainingData, 1e-5)
      val specializedPosteriorGP: LowRankGaussianProcess[ThreeD] = GaussianProcess.regression(f.specializedGp, trainingData, 1e-5, false)

      val meanPosterior = posteriorGP.mean
      val meanPosteriorSpecialized = specializedPosteriorGP.mean
      val phi1Posterior = posteriorGP.eigenPairs(0)._2
      val phi1PosteriorSpezialized = specializedPosteriorGP.eigenPairs(0)._2

      // both posterior processes should give the same values at the specialized points
      for (pt <- f.specializedPoints.par) {
        for (d <- 0 until 3) {
          meanPosterior(pt)(d) should be(meanPosteriorSpecialized(pt)(d) plusOrMinus 1e-5)
          phi1Posterior(pt)(d) should be(phi1PosteriorSpezialized(pt)(d) plusOrMinus 1e-5)
        }
      }
    }

    it("yields the same covariance function as a normal gp") {
      val f = Fixture

      val specializedCov = f.specializedGp.cov
      val cov = f.gp.cov
      for (pt1 <- f.specializedPoints.par; pt2 <- f.specializedPoints) {
        val covGp = cov(pt1, pt2)
        val covSpecialized = specializedCov(pt1, pt2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGp(i, j) should be(covSpecialized(i, j) plusOrMinus 1e-5)
        }
      }
    }
  }

}
