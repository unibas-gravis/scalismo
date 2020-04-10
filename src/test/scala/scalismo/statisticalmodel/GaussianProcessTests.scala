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

import java.net.URLDecoder

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Gaussian
import scalismo.ScalismoTestSuite
import scalismo.common._
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain, StructuredPoints}
import scalismo.io.{StatismoIO, StatisticalModelIO}
import scalismo.kernels.{DiagonalKernel, GaussianKernel, MatrixValuedPDKernel}
import scalismo.numerics.{GridSampler, UniformSampler}
import scalismo.utils.Random

import scala.language.implicitConversions

class GaussianProcessTests extends ScalismoTestSuite {

  implicit val random = Random(42)

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  implicit def intToPointId(i: Int): PointId = PointId(i)

  implicit def intSeqToPointId(i: Seq[Int]): Seq[PointId] = i.map(PointId)

  describe("samples from a gaussian process") {

    def testVarianceForGP(gp: GaussianProcess[_1D, EuclideanVector[_1D]], domain: BoxDomain[_1D]): Unit = {
      val numPoints = 3
      val sampler = UniformSampler(domain, numPoints)

      val (pts, _) = sampler.sample.unzip

      val numSamples = 500
      def sampleValueForIthPoint(i: Int) = for (_ <- 0 until numSamples) yield {
        val ptData = gp.sampleAtPoints(UnstructuredPointsDomain(pts))
        ptData(i)(0)
      }

      // choose an arbitrary point of the domain and check that its mean and variance is correct

      def testAtIthPoint(i: Int) {
        val sampleValuesAtPt = sampleValueForIthPoint(i)
        val meanAtPt = sampleValuesAtPt.sum / numSamples
        val varAtPt = sampleValuesAtPt.foldLeft(0.0)((acc, e) => acc + (e - meanAtPt) * (e - meanAtPt)) / numSamples

        meanAtPt should be(0.0 +- 3e-1)
        varAtPt should be(1.0 +- 3e-1)
      }
      for (i <- 0 until numPoints) testAtIthPoint(i)
    }

    it("have the correct mean and variance for a full gp") {
      val domain = BoxDomain(Point(-1.0), Point(1.0))
      val m = Field(domain, (_: Point[_1D]) => EuclideanVector(0))
      val k = DiagonalKernel(GaussianKernel[_1D](2.0), 1)
      val gp = GaussianProcess[_1D, EuclideanVector[_1D]](m, k)
      testVarianceForGP(gp, domain)
    }

    it("have the correct mean and variance for a low-rank gp") {
      val domain = BoxDomain(Point(-1.0), Point(1.0))
      val m: Field[_1D, EuclideanVector[_1D]] with Object {
        val f: (Point[_1D]) => EuclideanVector[_1D]; def domain: Domain[_1D]
      } = Field(domain, (_: Point[_1D]) => EuclideanVector(0))
      val k: DiagonalKernel[_1D] = DiagonalKernel(GaussianKernel[_1D](2.0), 1)
      val sampler = UniformSampler(domain, 500)
      val lgp = LowRankGaussianProcess.approximateGPNystrom(GaussianProcess(m, k), sampler, 100)

      testVarianceForGP(lgp, domain)
    }

    it("yields the same values as the original gp at the given points") {
      val domain = BoxDomain(Point(-2.0f, 1.0f), Point(1.0f, 2.0f))
      val m = Field(domain, (_: Point[_2D]) => EuclideanVector(0.0f, 0.0f))
      val gp = GaussianProcess(m, DiagonalKernel(GaussianKernel[_2D](1.0), 2))

      val testPts = IndexedSeq(Point(-1.0f, 1.5f), Point(0.0f, 1.7f))
      val discreteGP = gp.marginal(UnstructuredPointsDomain(testPts))

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
      val domain = BoxDomain(-5.0, 5.0)
      val kernel = DiagonalKernel(GaussianKernel[_1D](5), 1)
      val gp = GaussianProcess(Field(domain, (_: Point[_1D]) => EuclideanVector(0f)), kernel)
      val gpLowRank = LowRankGaussianProcess.approximateGPNystrom(gp, UniformSampler(domain, 500), 100)

      val trainingData = IndexedSeq((-3.0, 1.0), (-1.0, 3.0), (0.0, -1.0), (1.0, -1.0), (3.0, 0.0)).map(t =>
        (Point(t._1), EuclideanVector(t._2))
      )
      val posteriorGP = gp.posterior(trainingData, 1e-8)
      val posteriorGPLowRank = gpLowRank.posterior(trainingData, 1e-8)
      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) +- 1e-1)
        posteriorGPLowRank.mean(x)(0) should be(y(0) +- 1e-1)
      }

    }

    it("yields a larger posterior variance for points that are less strongly constrained") {
      val domain = BoxDomain(-5.0, 5.0)
      val kernel = DiagonalKernel(GaussianKernel[_1D](1.0), 1)
      val gp = GaussianProcess(Field(domain, (_: Point[_1D]) => EuclideanVector(0f)), kernel)
      val gpLowRank = LowRankGaussianProcess.approximateGPNystrom(gp, UniformSampler(domain, 500), 100)

      val pt1 = -3.0
      val val1 = 1.0
      val pt2 = 1.0
      val val2 = -1.0
      def errorForSigma(sigma2: Double) = {
        MultivariateNormalDistribution(DenseVector(0.0), DenseMatrix.eye[Double](1) * sigma2)
      }
      val trainingData = IndexedSeq((pt1, val1, 0.1), (pt2, val2, 2.0))
        .map(t => (Point(t._1), EuclideanVector(t._2), errorForSigma(t._3)))
      val posteriorGPLowRank = gpLowRank.posterior(trainingData)
      val posteriorGP = gp.posterior(trainingData)

      posteriorGPLowRank.cov(pt1, pt1)(0, 0) should be < posteriorGPLowRank.cov(pt2, pt2)(0, 0)
      posteriorGP.cov(pt1, pt1)(0, 0) should be < posteriorGP.cov(pt2, pt2)(0, 0)
    }

    it("keeps the landmark points fixed for a 2D case") {
      val domain = BoxDomain((-5.0, -5.0), (5.0, 5.0))
      val gp = GaussianProcess[_2D, EuclideanVector[_2D]](DiagonalKernel(GaussianKernel[_2D](5), 2))
      val gpLowRank =
        LowRankGaussianProcess.approximateGPNystrom[_2D, EuclideanVector[_2D]](gp, UniformSampler(domain, 400), 100)

      val trainingData =
        IndexedSeq((Point(-3.0, -3.0), EuclideanVector(1.0, 1.0)), (Point(-1.0, 3.0), EuclideanVector(0.0, -1.0)))

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
      val domain = BoxDomain((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val gp = GaussianProcess[_3D, EuclideanVector[_3D]](DiagonalKernel(GaussianKernel[_3D](5), 3))
      val gpLowRank = LowRankGaussianProcess
        .approximateGPNystrom[_3D, EuclideanVector[_3D]](gp, UniformSampler(domain, 6 * 6 * 6), 50)

      val trainingData = IndexedSeq((Point(-3.0, -3.0, -1.0), EuclideanVector(1.0, 1.0, 2.0)),
                                    (Point(-1.0, 3.0, 0.0), EuclideanVector(0.0, -1.0, 0.0)))
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

  describe("a Gaussian process marginal likelihood") {

    object Fixture {
      val domain = BoxDomain((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val gp = GaussianProcess[_3D, EuclideanVector[_3D]](DiagonalKernel(GaussianKernel[_3D](5), 3))

      val moreComplexGP = GaussianProcess[_3D, EuclideanVector[_3D]](
        DiagonalKernel(GaussianKernel[_3D](5) + GaussianKernel[_3D](2.5) * 2 + GaussianKernel[_3D](0.5) * 1, 3)
      )

      val littleNoise = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3))
      val discreteDomain = UnstructuredPointsDomain(IndexedSeq(Point(-3.0, -3.0, -1.0), Point(-1.0, 3.0, 0.0)))
      val dataOnMean = discreteDomain.pointSet.points.toIndexedSeq.map { p =>
        (p, EuclideanVector(0.0, 0.0, 0.0), littleNoise)
      }
    }

    it("is higher for observations on the mean than 20 random samples") {

      val meanMarginal = GaussianProcess.marginalLikelihood(Fixture.gp, Fixture.dataOnMean)

      val sampleMarginals = (0 until 20).map { i =>
        val sampleDef = Fixture.gp.sampleAtPoints(Fixture.discreteDomain)
        val trainingData = sampleDef.pointsWithValues.map { case (p, v) => (p, v, Fixture.littleNoise) }
        GaussianProcess.marginalLikelihood(Fixture.gp, trainingData.toIndexedSeq)
      }

      assert(meanMarginal >= sampleMarginals.max)
    }

    it("penalizes more complex models on equally likely data") {

      val meanMarginalSimple = GaussianProcess.marginalLikelihood(Fixture.gp, Fixture.dataOnMean)
      val meanMarginalComplex = GaussianProcess.marginalLikelihood(Fixture.moreComplexGP, Fixture.dataOnMean)

      assert(meanMarginalSimple >= meanMarginalComplex)
    }

  }

  describe("a lowRankGaussian process") {
    object Fixture {
      val domain = BoxDomain((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = GridSampler(DiscreteImageDomain(domain.origin, domain.extent * (1.0 / 7), IntVector(7, 7, 7)))
      val kernel = DiagonalKernel(GaussianKernel[_3D](10), 3)
      val gp = {
        LowRankGaussianProcess.approximateGPNystrom[_3D, EuclideanVector[_3D]](
          GaussianProcess(Field(domain, (_: Point[_3D]) => EuclideanVector(0.0, 0.0, 0.0)), kernel),
          sampler,
          200
        )
      }
    }

    it("a sample created with the coefficients yields the right coefficients") {
      val gp = Fixture.gp
      val coeffs = DenseVector.rand[Double](gp.rank)
      val randInst = gp.instance(coeffs)
      val pts = Fixture.sampler.sample.map(_._1)
      val td = pts.map(pt => (pt, randInst(pt)))
      val computedCoeffs = gp.coefficients(td, 1e-8)
      computedCoeffs.size should equal(coeffs.size)

      for (i <- 0 until coeffs.size) {
        computedCoeffs(i) should be(coeffs(i) +- 1e-2)
      }
    }

    it("has the right rank when reduced") {
      val gp = Fixture.gp

      val newRank = gp.rank / 2
      val truncatedGP = gp.truncate(newRank)
      truncatedGP.rank should equal(newRank)

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
      val fewPointsSampler =
        GridSampler(DiscreteImageDomain(f.domain.origin, f.domain.extent * (1.0 / 8.0), IntVector(2, 2, 2)))
      val pts = fewPointsSampler.sample.map(_._1)
      for (pt1 <- pts.par; pt2 <- pts) {
        val covGP = f.gp.cov(pt1, pt2)
        val covKernel = f.kernel(pt1, pt2)
        for (i <- 0 until 3; j <- 0 until 3) {
          covGP(i, j) should be(covKernel(i, j) +- 1e-2f)
        }
      }
    }

    it(
      "yields the same covariance as given by the kernel for a real matrix valued kernel (with nondiagonal block structure)"
    ) {

      val covKernel = new MatrixValuedPDKernel[_3D] {
        val f0 = (pt: Point[_3D]) => pt.toBreezeVector
        val f1 = (pt: Point[_3D]) => Point(pt(0) * 2, pt(1) * 0.5, pt(2) * 3).toBreezeVector
        val f = (pt1: Point[_3D], pt2: Point[_3D]) => {
          f0(pt1).asDenseMatrix.t * f0(pt2).asDenseMatrix + f1(pt1).asDenseMatrix.t * f1(pt2).asDenseMatrix
        }

        override val domain = RealSpace[_3D]

        override def k(x: Point[_3D], y: Point[_3D]) = {
          f(x, y)
        }

        override def outputDim = 3
      }

      val domain = BoxDomain((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = UniformSampler(domain, 7 * 7 * 7)
      val kernel = covKernel
      val gp = {
        LowRankGaussianProcess.approximateGPNystrom[_3D, EuclideanVector[_3D]](
          GaussianProcess(Field(domain, (_: Point[_3D]) => EuclideanVector(0.0, 0.0, 0.0)), kernel),
          sampler,
          5
        )
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

      val domain = BoxDomain((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = UniformSampler(domain, 6 * 6 * 6)
      val mean = Field[_3D, EuclideanVector[_3D]](RealSpace[_3D], _ => EuclideanVector(0.0, 0.0, 0.0))
      val gp = GaussianProcess(mean, DiagonalKernel(GaussianKernel[_3D](5), 3))
      val lowRankGp = LowRankGaussianProcess.approximateGPNystrom(gp, sampler, 100)

      val discretizationPoints = sampler.sample.map(_._1)
      val discreteGP = DiscreteLowRankGaussianProcess(UnstructuredPointsDomain(discretizationPoints), lowRankGp)

      val coeffs = DenseVector.zeros[Double](lowRankGp.klBasis.size)
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

  describe("a LowRank Gaussian Process computed with the pivoted cholesky") {

    it("approximate the right amount of variance based on the relative error") {
      val ssmPath = getClass.getResource("/facemodel.h5").getPath
      val ssm = StatisticalModelIO.readStatisticalMeshModel(new java.io.File(URLDecoder.decode(ssmPath, "UTF-8"))).get
      val gpToApproximate = ssm.gp.interpolate(NearestNeighborInterpolator())

      val origVariance = gpToApproximate.klBasis.map(_.eigenvalue).sum

      for (epsilon <- Seq(0.0, 0.1, 0.2, 0.5)) {
        val approximatedGP =
          LowRankGaussianProcess.approximateGPCholesky(ssm.referenceMesh,
                                                       gpToApproximate,
                                                       epsilon,
                                                       NearestNeighborInterpolator())

        val approxVariance = approximatedGP.klBasis.map(_.eigenvalue).sum

        (origVariance - approxVariance) should be <= (epsilon * origVariance + 1e-1)
      }
    }

    it("keeps the probability of samples unchanged") {
      val ssmPath = getClass.getResource("/facemodel.h5").getPath
      val fullSsm = StatisticalModelIO.readStatisticalMeshModel(new java.io.File(URLDecoder.decode(ssmPath, "UTF-8"))).get

      // we truncate the ssm to avoid numerical error
      val ssm = fullSsm.truncate(fullSsm.rank / 2)

      val gpToApproximate = ssm.gp.interpolate(NearestNeighborInterpolator())
      val approximatedGP =
        LowRankGaussianProcess.approximateGPCholesky(ssm.referenceMesh,
                                                     gpToApproximate,
                                                     0.0,
                                                     NearestNeighborInterpolator())

      val rank = gpToApproximate.rank
      for (i <- 0 until 10) yield {
        val trueCoeffs = DenseVector.rand(rank, breeze.stats.distributions.Gaussian(0, 1))
        val sample = ssm.gp.instance(trueCoeffs)
        val dataPoints = sample.domain.pointSet.points.toIndexedSeq.zip(sample.values.toIndexedSeq)
        val coeffsApproximatedGp = approximatedGP.coefficients(dataPoints, 1e-5)

        // as the probability is fully defined by the norm of the coefficient vector, it is sufficient to
        // compare the norms
        breeze.linalg.norm(coeffsApproximatedGp) should be(breeze.linalg.norm(trueCoeffs) +- 1e-1)
      }

    }

  }

  describe("a discrete LowRank Gaussian process") {

    object Fixture {
      val domain = BoxDomain((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = UniformSampler(domain, 6 * 6 * 6)
      val mean = Field[_3D, EuclideanVector[_3D]](RealSpace[_3D], _ => EuclideanVector(0.0, 0.0, 0.0))
      val gp = GaussianProcess(mean, DiagonalKernel(GaussianKernel[_3D](5), 3))

      val lowRankGp = LowRankGaussianProcess.approximateGPNystrom(gp, sampler, 200)

      val discretizationPoints = sampler.sample.map(_._1)
      val discreteLowRankGp = DiscreteLowRankGaussianProcess(UnstructuredPointsDomain(discretizationPoints), lowRankGp)

      val trainingData = IndexedSeq((0, EuclideanVector.zeros[_3D]),
                                    (discretizationPoints.size / 2, EuclideanVector.zeros[_3D]),
                                    (discretizationPoints.size - 1, EuclideanVector.zeros[_3D])).map {
        case (i, v) => (PointId(i), v)
      }
      val cov = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * 1e-5)

      val trainingDataDiscreteGP = trainingData.map { case (ptId, v) => (ptId, v, cov) }
      val trainingDataGP = trainingData.map { case (ptId, v)         => (discretizationPoints(ptId.id), v) }
      val trainingDataLowRankGP = trainingDataDiscreteGP.map {
        case (ptId, v, cov) => (discreteLowRankGp.domain.pointSet.point(ptId), v, cov)
      }

    }

    it("will yield the correct values at the interpolation points when it is interpolated") {
      val f = Fixture
      val gp = f.discreteLowRankGp.interpolateNystrom(100)
      val discreteGp = gp.discretize(UnstructuredPointsDomain(f.discretizationPoints))

      val gaussRNG = Gaussian(0, 1)(random.breezeRandBasis)
      val coeffs = DenseVector.rand(gp.rank, gaussRNG)

      val sample = gp.instance(coeffs)
      val discreteSample = discreteGp.instance(coeffs)
      for ((pt, vec) <- discreteSample.pointsWithValues) {
        (sample(pt) - vec).norm should be(0.0 +- 1e-5)
      }
    }

    it("yields the same result for gp regression as a LowRankGaussianProcess") {
      val f = Fixture

      val posteriorGP = f.lowRankGp.posterior(f.trainingDataGP, 1e-5)
      val discretePosteriorGP = DiscreteLowRankGaussianProcess.regression(f.discreteLowRankGp, f.trainingDataDiscreteGP)

      val meanPosterior = posteriorGP.mean
      val meanPosteriorSpecialized = discretePosteriorGP.mean
      val phi1Posterior = posteriorGP.klBasis(0).eigenfunction
      val phi1PosteriorSpecialized = discretePosteriorGP.klBasis(0).eigenfunction

      // both posterior processes should give the same values at the specialized points
      for ((pt, id) <- f.discretizationPoints.zipWithIndex) {
        for (d <- 0 until 3) {
          meanPosterior(pt)(d) should be(meanPosteriorSpecialized(id)(d) +- 1e-5)
          phi1Posterior(pt)(d) should be(phi1PosteriorSpecialized(id)(d) +- 1e-5)
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

      val dgp1 = f.discreteLowRankGp.marginal(Seq(0, 1, 2))
      val dgp2 = dgp1.marginal(Seq(1))
      val dgp3 = f.discreteLowRankGp.marginal(Seq(1))

      DiscreteField.vectorize[_3D, UnstructuredPointsDomain, EuclideanVector[_3D]](dgp2.mean) should equal(
        DiscreteField.vectorize[_3D, UnstructuredPointsDomain, EuclideanVector[_3D]](dgp3.mean)
      )
      dgp2.cov.asBreezeMatrix should equal(dgp3.cov.asBreezeMatrix)
      dgp2.domain should equal(dgp3.domain)
    }

    it("yields the same result for the marginal as the discrete gp") {
      val f = Fixture

      val domain = UnstructuredPointsDomain(f.discretizationPoints)
      val dgp1 = f.lowRankGp.marginal(domain).marginal(Seq(0, 1, 2))
      val dgp2 = f.discreteLowRankGp.marginal(Seq(0, 1, 2))
      DiscreteField.vectorize[_3D, UnstructuredPointsDomain, EuclideanVector[_3D]](dgp1.mean) should equal(
        DiscreteField.vectorize[_3D, UnstructuredPointsDomain, EuclideanVector[_3D]](dgp2.mean)
      )
      dgp1.cov.asBreezeMatrix should equal(dgp2.cov.asBreezeMatrix)
      dgp1.domain should equal(dgp2.domain)
    }

    it("logpdf of the mean is higher than 10 random samples") {
      val f = Fixture
      val discreteGP = f.discreteLowRankGp

      val logpdfmean = discreteGP.logpdf(discreteGP.mean)

      (0 until 10) foreach { _ =>
        val logpdfSample = discreteGP.logpdf(discreteGP.sample)
        logpdfmean should be >= logpdfSample
      }

    }

    it("yields the same values on the discrete points when interpolated with nearest neighbor") {
      val f = Fixture
      val interpolatedGP = f.discreteLowRankGp.interpolate(NearestNeighborInterpolator())
      val gaussRNG = Gaussian(0, 1)(random.breezeRandBasis)
      val coeffs = DenseVector.rand(interpolatedGP.rank, gaussRNG)

      val discreteInstance = f.discreteLowRankGp.instance(coeffs)
      val contInterpolatedInstance = interpolatedGP.instance(coeffs)
      f.discreteLowRankGp.domain.pointSet.pointsWithId.foreach {
        case (p, i) => discreteInstance(i) shouldBe contInterpolatedInstance(p)
      }
    }

    it("yields the same posterior values on the discrete points when interpolated with nearest neighbor") {
      val f = Fixture

      val orignalLowRankPosterior = f.lowRankGp.posterior(f.trainingDataLowRankGP)
      val interpolatedGPPosterior =
        f.discreteLowRankGp.interpolate(NearestNeighborInterpolator()).posterior(f.trainingDataLowRankGP)

      val gaussRNG = Gaussian(0, 1)(random.breezeRandBasis)
      val coeffs = DenseVector.rand(orignalLowRankPosterior.rank, gaussRNG)

      val originalPosteriorInstance = orignalLowRankPosterior.instance(coeffs)
      val interpolatedPosteriorInstance = interpolatedGPPosterior.instance(coeffs)

      f.discreteLowRankGp.domain.pointSet.points.foreach { p =>
        val orig = originalPosteriorInstance(p)
        val intp = interpolatedPosteriorInstance(p)
        orig(0) shouldBe intp(0) +- 1.0e-10
        orig(1) shouldBe intp(1) +- 1.0e-10
        orig(2) shouldBe intp(2) +- 1.0e-10
      }
    }

    it(
      "yields the same values for an instance at a point when either only the point or the complete instance is calculated"
    ) {
      val f = Fixture
      val gp = f.discreteLowRankGp
      val points = (0 until 1000) map { i =>
        random.scalaRandom.nextInt(gp._domain.pointSet.numberOfPoints)
      }
      points.foreach { pid =>
        val k = gp.rank
        val gaussRNG = Gaussian(0, 1)(random.breezeRandBasis)
        val coeffs = DenseVector.rand(k, gaussRNG)
        val instance = gp.instance(coeffs)
        instance.data(pid) shouldBe gp.instanceAtPoint(coeffs, pid)
      }
    }

  }

}
