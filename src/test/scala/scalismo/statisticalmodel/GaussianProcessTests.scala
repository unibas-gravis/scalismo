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
package scalismo.statisticalmodel

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{ BoxDomain, DiscreteDomain, RealSpace, VectorField }
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.image.DiscreteImageDomain
import scalismo.kernels.{ GaussianKernel, MatrixValuedPDKernel, UncorrelatedKernel }
import scalismo.numerics.{ GridSampler, UniformSampler }

import scala.language.implicitConversions

class GaussianProcessTests extends ScalismoTestSuite {
  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("samples from a gaussian process") {

    def testVarianceForGP(gp: GaussianProcess[_1D, _1D], domain: BoxDomain[_1D]): Unit = {
      val numPoints = 3
      val sampler = UniformSampler(domain, numPoints)

      val (pts, _) = sampler.sample.unzip

      val numSamples = 500
      def sampleValueForIthPoint(i: Int) = for (_ <- 0 until numSamples) yield {
        val ptData = gp.sampleAtPoints(pts)
        ptData(i)(0)
      }

      // choose an arbitrary point of the domain and check that its mean and variance is correct

      def testAtIthPoint(i: Int) {
        val sampleValuesAtPt = sampleValueForIthPoint(i)
        val meanAtPt = sampleValuesAtPt.sum / numSamples
        val varAtPt = (sampleValuesAtPt.foldLeft(0.0f)((acc, e) => acc + (e - meanAtPt) * (e - meanAtPt))) / numSamples

        meanAtPt should be(0.0f +- (3e-1f))
        varAtPt should be(1.0f +- (3e-1f))
      }
      for (i <- 0 until numPoints) testAtIthPoint(i)
    }

    it("have the correct mean and variance for a full gp") {
      val domain = BoxDomain[_1D](Point(-1.0), Point(1.0))
      val m = VectorField(domain, (_: Point[_1D]) => Vector(0))
      val k = UncorrelatedKernel[_1D](GaussianKernel[_1D](2.0))
      val gp = GaussianProcess[_1D, _1D](m, k)
      testVarianceForGP(gp, domain)
    }

    it("have the correct mean and variance for a low-rank gp") {
      val domain = BoxDomain[_1D](Point(-1.0), Point(1.0))
      val m = VectorField(domain, (_: Point[_1D]) => Vector(0))
      val k = UncorrelatedKernel[_1D](GaussianKernel[_1D](2.0))
      val sampler = UniformSampler(domain, 500)
      val lgp = LowRankGaussianProcess.approximateGP(GaussianProcess(m, k), sampler, 100)

      testVarianceForGP(lgp, domain)
    }

    it("yields the same values as the original gp at the given points") {
      val domain = BoxDomain[_2D](Point(-2.0f, 1.0f), Point(1.0f, 2.0f))
      val m = VectorField(domain, (_: Point[_2D]) => Vector(0.0f, 0.0f))
      val gp = GaussianProcess(m, UncorrelatedKernel[_2D](GaussianKernel[_2D](1.0)))

      val testPts = Seq(Point(-1.0f, 1.5f), Point(0.0f, 1.7f))
      val discreteGP = gp.marginal(testPts)

      for ((testPt, testPtId) <- testPts.zipWithIndex) {
        discreteGP.mean(testPtId) should equal(gp.mean(testPt))
        for ((testPt2, testPtId2) <- testPts.zipWithIndex) {
          discreteGP.cov(testPtId, testPtId2) should equal(gp.cov(testPt, testPt2))
        }
      }
    }

  }

  describe("A Gaussian process regression") {
    it("keeps the landmark points fixed for a 1D case") {
      val domain = BoxDomain[_1D](-5.0f, 5f)
      val kernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](5))
      val gp = GaussianProcess(VectorField(domain, (_: Point[_1D]) => Vector(0f)), kernel)
      val gpLowRank = LowRankGaussianProcess.approximateGP(gp, UniformSampler(domain, 500), 100)

      val trainingData = IndexedSeq((-3.0, 1.0), (-1.0, 3.0), (0.0, -1.0), (1.0, -1.0), (3.0, 0.0)).map(t => (Point(t._1), Vector(t._2)))
      val posteriorGP = gp.posterior(trainingData, 1e-8)
      val posteriorGPLowRank = gpLowRank.posterior(trainingData, 1e-8)
      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) +- 1e-1)
        posteriorGPLowRank.mean(x)(0) should be(y(0) +- 1e-1)
      }

    }

    it("yields a larger posterior variance for points that are less strongly constrained") {
      val domain = BoxDomain[_1D](-5.0f, 5f)
      val kernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](1.0))
      val gp = GaussianProcess(VectorField(domain, (_: Point[_1D]) => Vector(0f)), kernel)
      val gpLowRank = LowRankGaussianProcess.approximateGP(gp, UniformSampler(domain, 500), 100)

      val pt1 = -3.0f
      val val1 = 1.0
      val pt2 = 1.0f
      val val2 = -1.0
      def errorForSigma(sigma2: Double) = {
        NDimensionalNormalDistribution(Vector(0.0), SquareMatrix.eye[_1D] * sigma2)
      }
      val trainingData = IndexedSeq((pt1, val1, 0.1), (pt2, val2, 2.0))
        .map(t => (Point(t._1), Vector(t._2), errorForSigma(t._3)))
      val posteriorGPLowRank = gpLowRank.posterior(trainingData)
      val posteriorGP = gp.posterior(trainingData)

      posteriorGPLowRank.cov(pt1, pt1)(0, 0) should be < posteriorGPLowRank.cov(pt2, pt2)(0, 0)
      posteriorGP.cov(pt1, pt1)(0, 0) should be < posteriorGP.cov(pt2, pt2)(0, 0)
    }

    it("keeps the landmark points fixed for a 2D case") {
      val domain = BoxDomain[_2D]((-5.0f, -5.0f), (5.0f, 5.0f))
      val gp = GaussianProcess[_2D, _2D](VectorField(domain, _ => Vector(0.0, 0.0)),
        UncorrelatedKernel[_2D](GaussianKernel[_2D](5)))
      val gpLowRank = LowRankGaussianProcess.approximateGP[_2D, _2D](gp, UniformSampler(domain, 400), 100)

      val trainingData = IndexedSeq((Point(-3.0, -3.0), Vector(1.0, 1.0)), (Point(-1.0, 3.0), Vector(0.0, -1.0)))

      val posteriorGP = gp.posterior(trainingData, 1e-5)
      val posteriorGPLowRank = gpLowRank.posterior(trainingData, 1e-5)

      for ((x, y) <- trainingData) {
        posteriorGPLowRank.mean(x)(0) should be(y(0) +- 0.0001)
        posteriorGPLowRank.mean(x)(1) should be(y(1) +- 0.0001)
        posteriorGP.mean(x)(0) should be(y(0) +- 0.0001)
        posteriorGP.mean(x)(1) should be(y(1) +- 0.0001)

      }
    }

    it("keeps the landmark points fixed for a 3D case") {
      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val gp = GaussianProcess[_3D, _3D](VectorField(domain, _ => Vector(0.0, 0.0, 0.0)),
        UncorrelatedKernel[_3D](GaussianKernel[_3D](5)))
      val gpLowRank = LowRankGaussianProcess.approximateGP[_3D, _3D](gp, UniformSampler(domain, 6 * 6 * 6), 50)

      val trainingData = IndexedSeq((Point(-3.0, -3.0, -1.0), Vector(1.0, 1.0, 2.0)), (Point(-1.0, 3.0, 0.0), Vector(0.0, -1.0, 0.0)))
      val posteriorGPLowRank = gpLowRank.posterior(trainingData, 1e-5)
      val posteriorGP = gp.posterior(trainingData, 1e-5)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) +- 0.0001)
        posteriorGP.mean(x)(1) should be(y(1) +- 0.0001)
        posteriorGP.mean(x)(2) should be(y(2) +- 0.0001)
        posteriorGPLowRank.mean(x)(0) should be(y(0) +- 0.0001)
        posteriorGPLowRank.mean(x)(1) should be(y(1) +- 0.0001)
        posteriorGPLowRank.mean(x)(2) should be(y(2) +- 0.0001)

      }

    }
  }

  describe("a lowRankGaussian process") {
    object Fixture {
      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = GridSampler(DiscreteImageDomain(domain.origin, domain.extent * (1.0 / 7), Index(7, 7, 7)))
      val kernel = UncorrelatedKernel[_3D](GaussianKernel[_3D](10))
      val gp = {
        LowRankGaussianProcess.approximateGP[_3D, _3D](GaussianProcess(VectorField(domain, _ => Vector(0.0, 0.0, 0.0)), kernel), sampler, 200)
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
        computedCoeffs(i) should be(coeffs(i) +- 1e-2)
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
        for (i <- 0 until 3) {
          sampleDf(i) should be(projectedDf(i) +- 1e-2)
        }
      }
    }

    it("yields the same covariance as given by the kernel") {
      val f = Fixture
      val fewPointsSampler = GridSampler(DiscreteImageDomain(f.domain.origin, f.domain.extent * (1.0 / 8), Index(2, 2, 2)))
      val pts = fewPointsSampler.sample.map(_._1)
      for (pt1 <- pts.par; pt2 <- pts) {
        val covGP = f.gp.cov(pt1, pt2)
        val covKernel = f.kernel(pt1, pt2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGP(i, j) should be(covKernel(i, j) +- 1e-2f)
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

        override val domain = RealSpace[_3D]

        override def k(x: Point[_3D], y: Point[_3D]) = {
          SquareMatrix[_3D](f(x, y).data)
        }
      }

      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = UniformSampler(domain, 7 * 7 * 7)
      val kernel = covKernel
      val gp = {
        LowRankGaussianProcess.approximateGP[_3D, _3D](GaussianProcess(VectorField(domain, _ => Vector(0.0, 0.0, 0.0)), kernel), sampler, 5)
      }
      val fewPointsSampler = UniformSampler(domain, 2 * 2 * 2)
      val pts = fewPointsSampler.sample.map(_._1)

      for (pt1 <- pts; pt2 <- pts) {
        val covGP = gp.cov(pt1, pt2)
        val covKernel = kernel(pt1, pt2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGP(i, j) should be(covKernel(i, j) +- 1e-2)
        }
      }
    }

    it("can be discretized and yields the correct values at the discretization points") {

      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = UniformSampler(domain, 6 * 6 * 6)
      val mean = VectorField[_3D, _3D](RealSpace[_3D], _ => Vector(0.0, 0.0, 0.0))
      val gp = GaussianProcess(mean, UncorrelatedKernel[_3D](GaussianKernel[_3D](5)))
      val lowRankGp = LowRankGaussianProcess.approximateGP(gp, sampler, 100)

      val discretizationPoints = sampler.sample.map(_._1)
      val discreteGP = DiscreteLowRankGaussianProcess(DiscreteDomain.fromSeq(discretizationPoints), lowRankGp)

      val coeffs = DenseVector.zeros[Float](lowRankGp.klBasis.size)
      val gpInstance = lowRankGp.instance(coeffs)
      val discreteInstance = discreteGP.instance(coeffs)
      for ((pt, df) <- discreteInstance.pointsWithValues) {
        gpInstance(pt) should equal(df)
      }

      for ((pt, df) <- discreteGP.instance(coeffs).pointsWithValues) {
        df should equal(gpInstance(pt))
      }
    }

  }

  describe("a discrete LowRank Gaussian process") {

    object Fixture {
      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = UniformSampler(domain, 6 * 6 * 6)
      val mean = VectorField[_3D, _3D](RealSpace[_3D], _ => Vector(0.0, 0.0, 0.0))
      val gp = GaussianProcess(mean, UncorrelatedKernel[_3D](GaussianKernel[_3D](5)))

      val lowRankGp = LowRankGaussianProcess.approximateGP(gp, sampler, 200)

      val discretizationPoints = sampler.sample.map(_._1)
      val discreteLowRankGp = DiscreteLowRankGaussianProcess(DiscreteDomain.fromSeq(discretizationPoints), lowRankGp)
    }

    it("will yield the correct values at the interpolation points when it is interpolated") {
      val f = Fixture
      val gp = f.discreteLowRankGp.interpolateNystrom(100)
      val discreteGp = gp.discretize(f.discretizationPoints)

      val gaussRNG = breeze.stats.distributions.Gaussian(0, 1)
      val coeffs = DenseVector.rand(gp.rank, gaussRNG).map(_.toFloat)

      val sample = gp.instance(coeffs)
      val discreteSample = discreteGp.instance(coeffs)
      for ((pt, vec) <- discreteSample.pointsWithValues) {
        (sample(pt) - vec).norm should be(0.0 +- 1e-5)
      }
    }

    it("yields the same result for gp regression as a LowRankGaussianProcess") {
      val f = Fixture

      val trainingData = IndexedSeq((0, Vector.zeros[_3D]), (f.discretizationPoints.size / 2, Vector.zeros[_3D]), (f.discretizationPoints.size - 1, Vector.zeros[_3D]))
      val cov = NDimensionalNormalDistribution(Vector.zeros[_3D], SquareMatrix.eye[_3D] * 1e-5)
      val trainingDataDiscreteGP = trainingData.map { case (ptId, v) => (ptId, v, cov) }
      val trainingDataGP = trainingData.map { case (ptId, v) => (f.discretizationPoints(ptId), v) }

      val posteriorGP = f.lowRankGp.posterior(trainingDataGP, 1e-5)
      val discretePosteriorGP = DiscreteLowRankGaussianProcess.regression(f.discreteLowRankGp, trainingDataDiscreteGP)

      val meanPosterior = posteriorGP.mean
      val meanPosteriorSpecialized = discretePosteriorGP.mean
      val phi1Posterior = posteriorGP.klBasis(0)._2
      val phi1PosteriorSpezialized = discretePosteriorGP.klBasis(0)._2

      // both posterior processes should give the same values at the specialized points
      for ((pt, id) <- f.discretizationPoints.zipWithIndex) {
        for (d <- 0 until 3) {
          meanPosterior(pt)(d) should be(meanPosteriorSpecialized(id)(d) +- 1e-5)
          phi1Posterior(pt)(d) should be(phi1PosteriorSpezialized(id)(d) +- 1e-5)
        }
      }
    }

    it("yields the same covariance function as a normal gp") {
      val f = Fixture

      val discreteGPCov = f.discreteLowRankGp.cov
      val cov = f.lowRankGp.cov

      for ((pt1, ptId1) <- f.discretizationPoints.zipWithIndex; (pt2, ptId2) <- f.discretizationPoints.zipWithIndex) {
        val covGp = cov(pt1, pt2)
        val covDiscrete = discreteGPCov(ptId1, ptId2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGp(i, j) should be(covDiscrete(i, j) +- 1e-5)
        }
      }
    }

    it("yeilds the same result when marginalized the points in one or two steps") {
      val f = Fixture

      val pts = f.discretizationPoints
      val dgp1 = f.discreteLowRankGp.marginal(Seq(0, 1, 2))
      val dgp2 = dgp1.marginal(Seq(1))
      val dgp3 = f.discreteLowRankGp.marginal(Seq(1))

      dgp2.mean.asBreezeVector should equal(dgp3.mean.asBreezeVector)
      dgp2.cov.asBreezeMatrix should equal(dgp3.cov.asBreezeMatrix)
      dgp2.domain should equal(dgp3.domain)
    }

    it("yields the same result for the marginal as the discretegp") {
      val f = Fixture

      val pts = f.discretizationPoints
      val dgp1 = f.lowRankGp.marginal(pts).marginal(Seq(0, 1, 2))
      val dgp2 = f.discreteLowRankGp.marginal(Seq(0, 1, 2))
      dgp1.mean.asBreezeVector should equal(dgp2.mean.asBreezeVector)
      dgp1.cov.asBreezeMatrix should equal(dgp2.cov.asBreezeMatrix)
      dgp1.domain should equal(dgp2.domain)
    }

    it("pdf of the mean is higher than 10 random samples") {
      val f = Fixture

      val discreteGP = f.discreteLowRankGp
      val pdfmean = discreteGP.pdf(discreteGP.mean)
      val s = (0 until 10) map { _ =>
        val pdfSample = discreteGP.pdf(discreteGP.sample)
        pdfmean >= pdfSample
      }

      assert(s.forall(e => e))
    }

  }

}
