package org.statismo.stk.core.statisticalmodel

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

class MultivariateNormalDistributionTests extends FunSpec with ShouldMatchers {
  describe("A 1D Multivariate normal") {
    it("should give the same pdf values as breeze Gaussian with the same parameters") {
      val mvn = new MultivariateNormalDistribution(DenseVector(2f), DenseMatrix(3f))
      val n = breeze.stats.distributions.Gaussian(2, Math.sqrt(3f))
      for (pt <- Seq(0, 0.1, -2, 3.1)) {
        n.pdf(pt) should be(mvn.pdf(DenseVector(pt.toFloat)) plusOrMinus (1e-5))
      }
    }

  }


  describe("A 3D Multivariate normal") { 
    it("yields the right result when estimated from data") {
      // TODO
    }
  }

  describe("The mahalanobis distance") {
    it("is 0 if the mean is used") {
      val mu = DenseVector(2f, 1.0f)
      val cov = DenseMatrix.create[Float](2,2, Array(1, 0, 0, 1))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      mvn.mahalanobisDistance(mu) should be(0.0 plusOrMinus 1e-5)
    }

    it("yields the same as the squared norm if mean 0 and identity cov is used") {
      val mu = DenseVector(0f, 0f)
      val cov = DenseMatrix.create[Float](2,2, Array(1, 0, 0, 1))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      val x = DenseVector(3f, 7f)
      mvn.mahalanobisDistance(x) should be(x.norm(2)) 
    }

    it("increases with distance from the mean") {
      val mu = DenseVector(3f, 0f)
      val cov = DenseMatrix.create[Float](2,2, Array(1, 0, 0, 1))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      for (i <- 1 until 10) {
        mvn.mahalanobisDistance(mu + DenseVector(1f, i.toFloat)) should be > mvn.mahalanobisDistance(mu + DenseVector(1f, (i-1).toFloat))
        mvn.mahalanobisDistance(mu + DenseVector(i.toFloat, 1f)) should be > mvn.mahalanobisDistance(mu + DenseVector((i-1).toFloat, 1f))
      }
    }
    it("gives value 2 for a sample with 2 stddev") {
      val variance = 5f
      val stddev = math.sqrt(variance)
      val mvn = new MultivariateNormalDistribution(DenseVector(0f), DenseMatrix.create[Float](1, 1, Array(variance)))
      mvn.mahalanobisDistance(DenseVector(math.sqrt(variance).toFloat)) should be(1.0 plusOrMinus(1e-5))
    }
  }


  
  

}