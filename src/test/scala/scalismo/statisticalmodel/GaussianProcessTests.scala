package scalismo.statisticalmodel

import scalismo.common.{RealSpace, VectorField, BoxDomain}
import scalismo.image.DiscreteImageDomain
import scalismo.geometry._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.Vector.implicits._
import scalismo.geometry.Index.implicits._

import scalismo.kernels.{MatrixValuedPDKernel, GaussianKernel, UncorrelatedKernel}
import scalismo.numerics.{GridSampler, UniformSampler}
import scala.language.implicitConversions
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseVector
import java.io.File

class GaussianProcessTests extends FunSpec with Matchers {
  implicit def doubleToFloat(d: Double) = d.toFloat


  describe("samples from a gaussian process") {

    def testVarianceForGP(gp : GaussianProcess[_1D, _1D], domain : BoxDomain[_1D]): Unit = {
      val numPoints = 3
      val sampler = UniformSampler(domain, numPoints)

      val (pts, _) = sampler.sample.unzip

      val numSamples = 500
      def sampleValueForIthPoint(i : Int) = for (_ <- 0 until numSamples) yield {
        val ptData = gp.sampleAtPoints(pts)
        ptData(i)(0)
      }

      // choose an arbitrary point of the domain and check that its mean and variance is correct

      def testAtIthPoint(i : Int) {
        val sampleValuesAtPt = sampleValueForIthPoint(i)
        val meanAtPt= sampleValuesAtPt.sum / numSamples
        val varAtPt = (sampleValuesAtPt.foldLeft(0.0f)((acc, e) => acc + (e - meanAtPt) * (e - meanAtPt))) / numSamples

        meanAtPt should be(0.0f +- (3e-1f))
        varAtPt should be(1.0f +- (3e-1f))
      }
      for (i <- 0 until numPoints) testAtIthPoint(i)
    }

    it("have the correct mean and variance for a full gp") {
      val domain = BoxDomain[_1D](Point(-1.0), Point(1.0))
      val m = VectorField(domain, (_ : Point[_1D]) => Vector(0))
      val k = UncorrelatedKernel[_1D](GaussianKernel[_1D](2.0))
      val gp = GaussianProcess[_1D, _1D](m, k)
      testVarianceForGP(gp, domain)
    }

    it("have the correct mean and variance for a low-rank gp") {
      val domain = BoxDomain[_1D](Point(-1.0), Point(1.0))
      val m = VectorField(domain, (_ : Point[_1D]) => Vector(0))
      val k = UncorrelatedKernel[_1D](GaussianKernel[_1D](2.0))
      val sampler = UniformSampler(domain, 500)
      val lgp = LowRankGaussianProcess.approximateGP(GaussianProcess(m, k), sampler, 100)

      testVarianceForGP(lgp, domain)
    }


  }

  describe("A Gaussian process regression") {
    it("keeps the landmark points fixed for a 1D case") {
      val domain = BoxDomain[_1D](-5.0f, 5f)
      val kernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](5))
      val gp = LowRankGaussianProcess.approximateGP(GaussianProcess(VectorField(domain, (_ : Point[_1D]) => Vector(0f)), kernel), UniformSampler(domain, 500),  100)

      val trainingData = IndexedSeq((-3.0, 1.0), (-1.0, 3.0), (0.0, -1.0), (1.0, -1.0), (3.0, 0.0)).map(t => (Point(t._1), Vector(t._2)))
      val posteriorGP = gp.posterior(trainingData, 1e-8)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) +- 1e-1)
      }

    }

    it("yields a larger posterior variance for points that are less strongly constrained") {
      val domain = BoxDomain[_1D](-5.0f, 5f)
      val kernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](1.0))
      val gp = LowRankGaussianProcess.approximateGP(GaussianProcess(VectorField(domain, (_ : Point[_1D]) => Vector(0f)), kernel), UniformSampler(domain, 500), 100)

      val pt1 = -3.0f
      val val1 = 1.0
      val pt2 = 1.0f
      val val2 = -1.0
      val trainingData = IndexedSeq((pt1, val1, 0.1), (pt2, val2, 2.0)).map(t => (Point(t._1), Vector(t._2), t._3))
      val posteriorGP = gp.posterior(trainingData)


      posteriorGP.cov(pt1, pt1)(0, 0) should be < posteriorGP.cov(pt2, pt2)(0, 0)
    }


    it("keeps the landmark points fixed for a 2D case") {
      val domain = BoxDomain[_2D]((-5.0f, -5.0f), (5.0f, 5.0f))
      val gp = LowRankGaussianProcess.approximateGP[_2D, _2D](GaussianProcess(VectorField(domain, _ => Vector(0.0, 0.0)), UncorrelatedKernel[_2D](GaussianKernel[_2D](5))), UniformSampler(domain, 400), 100)

      val trainingData = IndexedSeq((Point(-3.0, -3.0), Vector(1.0, 1.0)), (Point(-1.0, 3.0), Vector(0.0, -1.0)))
      val posteriorGP = gp.posterior(trainingData, 1e-5)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) +- 0.0001)
        posteriorGP.mean(x)(1) should be(y(1) +- 0.0001)
      }
    }

    it("keeps the landmark points fixed for a 3D case") {
      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val gp = LowRankGaussianProcess.approximateGP[_3D, _3D](GaussianProcess(VectorField(domain, _ => Vector(0.0, 0.0, 0.0)), UncorrelatedKernel[_3D](GaussianKernel[_3D](5))), UniformSampler(domain, 6 * 6 * 6), 50)

      val trainingData = IndexedSeq((Point(-3.0, -3.0, -1.0), Vector(1.0, 1.0, 2.0)), (Point(-1.0, 3.0, 0.0), Vector(0.0, -1.0, 0.0)))
      val posteriorGP = gp.posterior(trainingData, 1e-5)

      for ((x, y) <- trainingData) {
        posteriorGP.mean(x)(0) should be(y(0) +- 0.0001)
        posteriorGP.mean(x)(1) should be(y(1) +- 0.0001)
        posteriorGP.mean(x)(2) should be(y(2) +- 0.0001)
      }

    }
  }



  describe("a lowRankGaussian process") {
    object Fixture {
      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
      val sampler = GridSampler(DiscreteImageDomain(domain.origin, domain.extent * (1.0 / 7), Index(7, 7, 7)))
      val kernel = UncorrelatedKernel[_3D](GaussianKernel[_3D](10))
      val gp = {
       LowRankGaussianProcess.approximateGP[_3D, _3D](GaussianProcess(VectorField(domain, _ => Vector(0.0, 0.0, 0.0)), kernel),  sampler, 200)
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
      val fewPointsSampler = GridSampler(DiscreteImageDomain(f.domain.origin, f.domain.extent * (1.0 / 8), Index(2,2,2)))
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

  }
//
//  describe("a discrete Gaussian process") {
//
//
//    object Fixture {
//      val domain = BoxDomain[_3D]((-5.0f, -5.0f, -5.0f), (5.0f, 5.0f, 5.0f))
//      val sampler = UniformSampler(domain, 6 * 6 * 6)
//      val gp = {
//        LowRankGaussianProcess.createLowRankGaussianProcess[_3D](domain, sampler, _ => Vector(0.0, 0.0, 0.0), UncorrelatedKernel[_3D](GaussianKernel[_3D](5)), 100)
//      }
//      val discretizationPoints = sampler.sample.map(_._1)
//      val discreteGP = DiscreteLowRankGaussianProcess(discretizationPoints, gp)
//    }
//
//    it("yields the same deformations at the specialized points") {
//      val f = Fixture
//
//      val coeffs = DenseVector.zeros[Float](f.gp.eigenPairs.size)
//      val gpInstance = f.gp.instance(coeffs)
//      val discreteInstance = f.discreteGP.instance(coeffs)
//      for ((pt, df) <- discreteInstance) {
//        gpInstance(pt) should equal(df)
//      }
//
//      for ((pt, df) <- f.discreteGP.instance(coeffs)) {
//        df should equal(gpInstance(pt))
//      }
//    }
//
//
//    it("yields the same result for gp regression as a normal gp") {
//      val f = Fixture
//
//      //val trainingData = IndexedSeq((Point(-3.0, -3.0, -1.0), Vector(1.0, 1.0, 2.0)), (Point(-1.0, 3.0, 0.0), Vector(0.0, -1.0, 0.0)))
//      val trainingDataDiscreteGp = IndexedSeq(PointId(f.discretizationPoints(0)), PointId(f.discretizationPoints.size / 2), PointId(f.discretizationPoints.size - 1))
//      val trainingDataGP = trainingDataDiscreteGp.map(ptId => f.discretizationPoints(ptId.id))
//
//      val posteriorGP = GaussianProcess.regression(f.gp, trainingDataGP, 1e-5)
//      val discretePosteriorGP =  DiscreteLowRankGaussianProcess.regression(f.discreteGP, trainingDataDiscreteGp, 1e-5, meanOnly = false)
//
//      val meanPosterior = posteriorGP.mean
//      val meanPosteriorSpecialized = discretePosteriorGP.mean
//      val phi1Posterior = posteriorGP.eigenPairs(0)._2
//      val phi1PosteriorSpezialized = discretePosteriorGP.eigenPairs(0)._2
//
//      // both posterior processes should give the same values at the specialized points
//      for (pt <- f.specializedPoints.par) {
//        for (d <- 0 until 3) {
//          meanPosterior(pt)(d) should be(meanPosteriorSpecialized(pt)(d) +- 1e-5)
//          phi1Posterior(pt)(d) should be(phi1PosteriorSpezialized(pt)(d) +- 1e-5)
//        }
//      }
//    }
//
//
//    it("yields the same covariance function as a normal gp") {
//      val f = Fixture
//
//      val discreteGPCov = f.discreteGP.cov
//      val cov = f.gp.cov
//
//      for ((pt1, ptId1) <- f.discretizationPoints.zipWithIndex.par; (pt2, ptId2) <- f.discretizationPoints.zipWithIndex) {
//        val covGp = cov(pt1, pt2)
//        val covDiscrete = discreteGPCov(PointId(ptId1), PointId(ptId2))
//        for (i <- 0 until 3; j <- 0 until 3) {
//          covGp(i, j) should be(covDiscrete(i, j) +- 1e-5)
//        }
//      }
//    }
//
//  }

//  describe("a pca model, estimates the first eigenvalue from the samples") {
//    it("estimates the same variance for the first eigenmode independent of the discretization") {
//      org.statismo.stk.core.initialize()
//
//      // we create artifical samples from an existing model
//      val path = getClass.getResource("/facemodel.h5").getPath
//      val model = StatismoIO.readStatismoMeshModel(new File(path)).get
//
//      val samples = for (i <- 0 until 10) yield model.sample
//      val transforms = for (s <- samples) yield new Transformation[_3D] {
//        val samplePts = s.points.toIndexedSeq
//
//        override def apply(x: Point[_3D]): Point[_3D] = {
//          val (_, ptId) = model.referenceMesh.findClosestPoint(x)
//          samplePts(ptId)
//        }
//
//      }
//
//      // model building
//      val sampler1 = FixedPointsUniformMeshSampler3D(model.referenceMesh, 50000, 42)
//      val gp1 = LowRankGaussianProcess.createLowRankGPFromTransformations(model.referenceMesh.boundingBox, transforms, sampler1)
//
//      val sampler2 = FixedPointsUniformMeshSampler3D(model.referenceMesh, 100000, 42)
//      val gp2 = LowRankGaussianProcess.createLowRankGPFromTransformations(model.referenceMesh.boundingBox, transforms, sampler2)
//
//      val (lambdas1, _) = gp1.kltBasis.unzip
//      val (lambdas2, _) = gp2.kltBasis.unzip
//      for ((l1, l2) <- lambdas1 zip lambdas2 if l1 > 1e-5 && l2 > 1e-5) {
//        l1 should be(l2 +- (l1 * 0.05))
//      }
//    }
//  }



}
