package smptk
package statisticalmodel

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import geometry._
import geometry.implicits._
import numerics._
import image.DiscreteImageDomain1D
import breeze.linalg.DenseVector
import smptk.image.DiscreteImageDomain2D
import smptk.io.MeshIO
import smptk.image.DiscreteImageDomain3D
import java.io.File
import GaussianProcess._
import kernels._
import smptk.common.BoxedDomain1D
import smptk.common.BoxedDomain2D
import smptk.common.BoxedDomain3D

class GaussianProcessTests extends FunSpec with ShouldMatchers {
  describe("A Gaussian process regression") {
    ignore("keeps the landmark points fixed for a 1D case") {
      val domain = BoxedDomain1D(-5.0, 5)
      val config = LowRankGaussianProcessConfiguration[OneD](domain, UniformSampler1D(domain), _ => DenseVector(0.0), GaussianKernel1D(5), 100, 500)
      val gp = GaussianProcess.createLowRankGaussianProcess1D(config)

      val trainingData = IndexedSeq((-3.0, 1.0), (-1.0, 3.0), (0.0, -1.0), (1.0, -1.0), (3.0, 0.0)).map(t => (Point1D(t._1), DenseVector(t._2)))
      val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-8)

      for ((x, y) <- trainingData) {
        (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 1e-1))
      }
    }
  }

  ignore("keeps the landmark points fixed for a 2D case") {
    val domain = BoxedDomain2D((-5.0, -5.0), (5.0, 5.0))
    val config = LowRankGaussianProcessConfiguration[TwoD](domain, UniformSampler2D(domain), _ => DenseVector(0.0, 0.0), UncorrelatedKernelND(GaussianKernel2D(5), 2), 100, 200)
    val gp = GaussianProcess.createLowRankGaussianProcess2D(config)

    val trainingData = IndexedSeq((Point2D(-3.0, -3.0), DenseVector(1.0, 1.0)),( Point2D(-1.0, 3.0),  DenseVector(0.0, -1.0)))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
    }
  }


  ignore("keeps the landmark points fixed for a 3D case") {
    val domain = BoxedDomain3D((-5.0, -5.0, -5.0), (5.0, 5.0, 5.0))
    val config = LowRankGaussianProcessConfiguration[ThreeD](domain, UniformSampler3D(domain), _ => DenseVector(0.0, 0.0, 0.0), UncorrelatedKernelND(GaussianKernel3D(5), 3), 100, 400)
    val gp = GaussianProcess.createLowRankGaussianProcess3D(config)


    val trainingData = IndexedSeq((Point3D(-3.0, -3.0, -1.0), DenseVector(1.0, 1.0, 2.0)), (Point3D(-1.0, 3.0, 0.0), DenseVector(0.0, -1.0, 0.0)))
    val posteriorGP = GaussianProcess.regression(gp, trainingData, 1e-5)

    for ((x, y) <- trainingData) {
      (posteriorGP.mean(x)(0) should be(y(0) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(1) should be(y(1) plusOrMinus 0.0001))
      (posteriorGP.mean(x)(2) should be(y(2) plusOrMinus 0.0001))
    }


  }
  

}
