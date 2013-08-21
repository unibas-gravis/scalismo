package smptk.statisticalmodel

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.geometry._
import smptk.geometry.implicits._
import smptk.numerics._
import smptk.image.DiscreteImageDomain1D
import breeze.linalg.DenseVector
import smptk.image.DiscreteImageDomain2D
import smptk.io.MeshIO
import smptk.image.DiscreteImageDomain3D
import java.io.File
import GaussianProcess._
import smptk.kernels._
import smptk.common.BoxedDomain1D
import smptk.common.BoxedDomain2D
import smptk.common.BoxedDomain3D

class GaussianProcessTests extends FunSpec with ShouldMatchers {
  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("A Gaussian process regression") {
    it("keeps the landmark points fixed for a 1D case") {
      val domain = BoxedDomain1D(-5.0, 5)
      val kernel = UncorrelatedKernel1x1(GaussianKernel1D(5))
      val config = LowRankGaussianProcessConfiguration[OneD](domain, UniformSampler1D(domain), _ => Vector1D(0f), kernel, 100, 500)
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
    val config = LowRankGaussianProcessConfiguration[TwoD](domain, UniformSampler2D(domain), _ => Vector2D(0.0, 0.0), UncorrelatedKernel2x2(GaussianKernel2D(5)), 100, 400)
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
    val config = LowRankGaussianProcessConfiguration[ThreeD](domain, UniformSampler3D(domain), _ => Vector3D(0.0, 0.0, 0.0), UncorrelatedKernel3x3(GaussianKernel3D(5)), 100, 8 * 8 * 8)
    val gp = GaussianProcess.createLowRankGaussianProcess3D(config)

    val trainingData = IndexedSeq((Point3D(-3.0, -3.0, -1.0), Vector3D(1.0, 1.0, 2.0)), (Point3D(-1.0, 3.0, 0.0), Vector3D(0.0, -1.0, 0.0)))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(2) should be(y(2) plusOrMinus 0.0001))
    }

  }

  describe("a specialized Gaussian process") {
    it("yields the same deformations at the specialized points") {
      val domain = BoxedDomain3D((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
      val sampler = UniformSampler3D(domain)
      val config = LowRankGaussianProcessConfiguration[ThreeD](domain, sampler, _ => Vector3D(0.0, 0.0, 0.0), UncorrelatedKernel3x3(GaussianKernel3D(5)), 100, 6 * 6 * 6)
      val gp = GaussianProcess.createLowRankGaussianProcess3D(config)
      val points = sampler.sample(5 * 5 * 5).map(_._1)
      val specializedGp = gp.specializeForPoints(points)
      val coeffs = DenseVector.zeros[Float](gp.eigenPairs.size)
      val gpInstance = gp.instance(coeffs)
      val specializedGpInstance = specializedGp.instance(coeffs)
      for (pt <- points) {
        gpInstance(pt) should equal(specializedGpInstance(pt))
      }

      for ((pt, df) <- specializedGp.instanceAtPoints(coeffs)) {
        df should equal(gpInstance(pt))
      }
    }
  }

}
