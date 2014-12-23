package org.statismo.stk.core.statisticalmodel

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.Point.implicits._
import org.statismo.stk.core.numerics._
import breeze.linalg.DenseVector
import java.io.File
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.common.BoxedDomain

import org.statismo.stk.core.io.StatismoIO
import org.statismo.stk.core.registration.Transformation

class GaussianProcessTests extends FunSpec with ShouldMatchers {
  implicit def doubleToFloat(d: Double) = d.toFloat


  describe("samples from a gaussian process") {

    def testVarianceForGP(gp : GaussianProcess[_1D], domain : BoxedDomain[_1D]): Unit = {
      val numPoints = 3
      val sampler = UniformDistributionRandomSampler1D(domain, numPoints)
      val (pts, _) = sampler.sample.unzip

      val numSamples = 500
      def sampleValueForIthPoint(i : Int) = for (_ <- 0 until numSamples) yield {
        val (_, vecs) = gp.sampleAtPoints(pts).unzip
        vecs(i)(0)
      }

      // choose an arbitrary point of the domain and check that its mean and variance is correct

      def testAtIthPoint(i : Int) {
        val sampleValuesAtPt = sampleValueForIthPoint(i)
        val meanAtPt= sampleValuesAtPt.sum / numSamples
        val varAtPt = (sampleValuesAtPt.foldLeft(0.0f)((acc, e) => acc + (e - meanAtPt) * (e - meanAtPt))) / numSamples

        meanAtPt should be(0.0f plusOrMinus (3e-1f))
        varAtPt should be(1.0f plusOrMinus (3e-1f))
      }
      for (i <- 0 until numPoints) testAtIthPoint(i)
    }

    it("have the correct mean and variance for a full gp") {
      val domain = BoxedDomain[_1D](Point(-1.0), Point(1.0))
      val m = (_ : Point[_1D]) => Vector(0)
      val k = UncorrelatedKernel1x1(GaussianKernel1D(2.0))
      val gp = GaussianProcess[_1D](domain, m, k)
      testVarianceForGP(gp, domain)
    }

    it("have the correct mean and variance for a low-rank gp") {
      val domain = BoxedDomain[_1D](Point(-1.0), Point(1.0))
      val m = (_ : Point[_1D]) => Vector(0)
      val k = UncorrelatedKernel1x1(GaussianKernel1D(2.0))
      val sampler = UniformDistributionRandomSampler1D(domain, 500)
      val lgp = LowRankGaussianProcess.createLowRankGaussianProcess1D(LowRankGaussianProcessConfiguration(domain, sampler, m, k, 100))

      testVarianceForGP(lgp, domain)
    }

    it("have the correct mean and variance for a specialized low-rank gp") {

      val domain = BoxedDomain[_1D](Point(-1.0), Point(1.0))
      val m = (_ : Point[_1D]) => Vector(0)
      val k = UncorrelatedKernel1x1(GaussianKernel1D(2.0))
      val sampler = UniformDistributionRandomSampler1D(domain, 500)
      val (pts, _) = sampler.sample.unzip
      val lgp = LowRankGaussianProcess.createLowRankGaussianProcess1D(LowRankGaussianProcessConfiguration(domain, sampler, m, k, 100))

      testVarianceForGP(lgp.specializeForPoints(pts), domain)

    }

  }

  describe("A Gaussian process regression") {
    it("keeps the landmark points fixed for a 1D case") {
      val domain = BoxedDomain[_1D](-5.0f, 5f)
      val kernel = UncorrelatedKernel1x1(GaussianKernel1D(5))
      val config = LowRankGaussianProcessConfiguration[_1D](domain, UniformSampler1D(domain, 500), _ => Vector(0f), kernel, 100)
      val gp = LowRankGaussianProcess.createLowRankGaussianProcess1D(config)

      val trainingData = IndexedSeq((-3.0, 1.0), (-1.0, 3.0), (0.0, -1.0), (1.0, -1.0), (3.0, 0.0)).map(t => (Point(t._1), Vector(t._2)))
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-8)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 1e-1)
      }

    }

    it("yields a larger posterior variance for points that are less strongly constrained") {
      val domain = BoxedDomain[_1D](-5.0f, 5f)
      val kernel = UncorrelatedKernel1x1(GaussianKernel1D(1.0))
      val config = LowRankGaussianProcessConfiguration[_1D](domain, UniformSampler1D(domain, 500), _ => Vector(0f), kernel, 100)
      val gp = LowRankGaussianProcess.createLowRankGaussianProcess1D(config)

      val pt1 = -3.0f
      val val1 = 1.0
      val pt2 = 1.0f
      val val2 = -1.0
      val trainingData = IndexedSeq((pt1, val1, 0.1), (pt2, val2, 2.0)).map(t => (Point(t._1), Vector(t._2), t._3))
      val posteriorGP = GaussianProcess.regression(gp, trainingData)


      posteriorGP.cov(pt1, pt1)(0, 0) should be < posteriorGP.cov(pt2, pt2)(0, 0)
    }


    it("keeps the landmark points fixed for a 2D case") {
      val domain = BoxedDomain[_2D]((-5.0f, -5.0f), (5.0f, 5.0f))
      val config = LowRankGaussianProcessConfiguration[_2D](domain, UniformSampler2D(domain, 400), _ => Vector(0.0, 0.0), UncorrelatedKernel2x2(GaussianKernel2D(5)), 100)
      val gp = LowRankGaussianProcess.createLowRankGaussianProcess2D(config)

      val trainingData = IndexedSeq((Point(-3.0, -3.0), Vector(1.0, 1.0)), (Point(-1.0, 3.0), Vector(0.0, -1.0)))
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001)
        posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001)
      }
    }

    it("keeps the landmark points fixed for a 3D case") {
      val domain = BoxedDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val config = LowRankGaussianProcessConfiguration[_3D](domain, UniformSampler3D(domain, 6 * 6 * 6), _ => Vector(0.0, 0.0, 0.0), UncorrelatedKernel3x3(GaussianKernel3D(5)), 50)
      val gp = LowRankGaussianProcess.createLowRankGaussianProcess3D(config)

      val trainingData = IndexedSeq((Point(-3.0, -3.0, -1.0), Vector(1.0, 1.0, 2.0)), (Point(-1.0, 3.0, 0.0), Vector(0.0, -1.0, 0.0)))
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001)
        posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001)
        posteriorGP.mean(x)(2) should be(y(2) plusOrMinus 0.0001)
      }

    }
  }



  describe("a lowRankGaussian process") {
    object Fixture {
      val domain = BoxedDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = UniformSampler3D(domain, 7 * 7 * 7)
      val kernel = UncorrelatedKernel3x3(GaussianKernel3D(10))
      val gp = {
        val config = LowRankGaussianProcessConfiguration[_3D](domain, sampler, _ => Vector(0.0, 0.0, 0.0), kernel, 100)
       LowRankGaussianProcess.createLowRankGaussianProcess3D(config)
      }
    }

    it("a sample created with the coefficients yields the right coefficients") {
      val gp = Fixture.gp
      val coeffs = DenseVector.rand[Double](gp.rank).map(_.toFloat)
      val randInst = gp.instance(coeffs)
      val pts = Fixture.sampler.sample.map(_._1)
      val td = pts.map(pt => (pt, randInst(pt)))
      val computedCoeffs = gp.coefficients(td, 1e-8)
      computedCoeffs.size should equal(coeffs.size)

      for (i <- 0 until coeffs.size) {
        computedCoeffs(i) should be(coeffs(i) plusOrMinus 1e-2)
      }
    }

    it("yields the same object when a sample from the model is projected") {
      val gp = Fixture.gp
      // TODO: sample() should arguably accept seed argument
      val sample = gp.sample

      val pts = Fixture.sampler.sample.map(_._1)
      val td = pts.map(pt => (pt, sample(pt)))

      val projection = gp.project(td)
      for (pt <- pts) {
        val sampleDf = sample(pt)
        val projectedDf = projection(pt)
        for (i <- 0 until gp.outputDimensionality) {
          sampleDf(i) should be(projectedDf(i) plusOrMinus 1e-2)
        }
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
          covGP(i, j) should be(covKernel(i, j) plusOrMinus 1e-2f)
        }
      }
    }

    it("yields the same covariance as given by the kernel for a real matrix valued kernel (with nondiagonal block structure)") {

      val covKernel = new MatrixValuedPDKernel[_3D, _3D] {
        val f0 = (pt: Point[_3D]) => pt.toBreezeVector
        val f1 = (pt: Point[_3D]) => Point(pt(0) * 2, pt(1) * 0.5, pt(2) * 3).toBreezeVector
        val f = (pt1: Point[_3D], pt2: Point[_3D]) => {
          f0(pt1).asDenseMatrix.t * f0(pt2).asDenseMatrix + f1(pt1).asDenseMatrix.t * f1(pt2).asDenseMatrix
        }

        def apply(x: Point[_3D], y: Point[_3D]) = {
          SquareMatrix[_3D](f(x, y).data)
        }
      }

      val domain = BoxedDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = UniformSampler3D(domain, 7 * 7 * 7)
      val kernel = covKernel
      val gp = {
        val config = LowRankGaussianProcessConfiguration[_3D](domain, sampler, _ => Vector(0.0, 0.0, 0.0), kernel, 5)
        LowRankGaussianProcess.createLowRankGaussianProcess3D(config)
      }
      val fewPointsSampler = UniformSampler3D(domain, 2 * 2 * 2)
      val pts = fewPointsSampler.sample.map(_._1)

      for (pt1 <- pts; pt2 <- pts) {
        val covGP = gp.cov(pt1, pt2)
        val covKernel = kernel(pt1, pt2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGP(i, j) should be(covKernel(i, j) plusOrMinus 1e-2)
        }
      }
    }

  }

  describe("a specialized Gaussian process") {

    object Fixture {
      val domain = BoxedDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = UniformSampler3D(domain, 6 * 6 * 6)
      val gp = {
        val config = LowRankGaussianProcessConfiguration[_3D](domain, sampler, _ => Vector(0.0, 0.0, 0.0), UncorrelatedKernel3x3(GaussianKernel3D(5)), 100)
        LowRankGaussianProcess.createLowRankGaussianProcess3D(config)
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

      val trainingData = IndexedSeq((Point(-3.0, -3.0, -1.0), Vector(1.0, 1.0, 2.0)), (Point(-1.0, 3.0, 0.0), Vector(0.0, -1.0, 0.0)))
      val posteriorGP = GaussianProcess.regression(f.gp, trainingData, 1e-5)
      val specializedPosteriorGP: LowRankGaussianProcess[_3D] = GaussianProcess.regression(f.specializedGp, trainingData, 1e-5, meanOnly = false)

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

  describe("a pca model, estimates the first eigenvalue from the samples") {
    it("estimates the same variance for the first eigenmode independent of the discretization") {
      org.statismo.stk.core.initialize()

      // we create artifical samples from an existing model
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val samples = for (i <- 0 until 10) yield model.sample
      val transforms = for (s <- samples) yield new Transformation[_3D] {
        val samplePts = s.pointSeq

        override def apply(x: Point[_3D]): Point[_3D] = {
          val (_, ptId) = model.mesh.findClosestPoint(x)
          samplePts(ptId)
        }

      }

      // model building
      val sampler1 = FixedPointsUniformMeshSampler3D(model.mesh, 50000, 42)
      val gp1 = LowRankGaussianProcess.createLowRankGPFromTransformations(model.mesh, transforms, sampler1)

      val sampler2 = FixedPointsUniformMeshSampler3D(model.mesh, 100000, 42)
      val gp2 = LowRankGaussianProcess.createLowRankGPFromTransformations(model.mesh, transforms, sampler2)

      val (lambdas1, _) = gp1.eigenPairs.unzip
      val (lambdas2, _) = gp2.eigenPairs.unzip
      for ((l1, l2) <- lambdas1 zip lambdas2 if l1 > 1e-5 && l2 > 1e-5) {
        l1 should be(l2 plusOrMinus (l1 * 0.05))
      }
    }
  }



}
