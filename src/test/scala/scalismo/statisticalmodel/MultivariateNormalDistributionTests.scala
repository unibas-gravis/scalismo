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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.ScalismoTestSuite
import scalismo.utils.Random

class MultivariateNormalDistributionTests extends ScalismoTestSuite {

  implicit val random: Random = Random(42)

  describe("A 1D Multivariate normal") {
    it("should give the same pdf values as breeze Gaussian with the same parameters") {
      val mvn = new MultivariateNormalDistribution(DenseVector(2.0), DenseMatrix(3.0))
      val n = breeze.stats.distributions.Gaussian(2, Math.sqrt(3.0))
      for (pt <- Seq(0.0, 0.1, -2.0, 3.1)) {
        n.pdf(pt) should be(mvn.pdf(DenseVector(pt)) +- 1e-5)
      }
    }

  }

  describe("A nD diagonal Multivariate normal") {
    it("should give the same logpdf values as breeze Gaussian with the same parameters to test numerical stability") {
      val n = 100
      val variance = 0.01
      val mean = DenseVector.zeros[Double](n)
      val cov = variance * DenseMatrix.eye[Double](n)
      val mvn = new MultivariateNormalDistribution(mean, cov)
      val breezeMvn = breeze.stats.distributions.MultivariateGaussian.apply(mean, cov)
      for (v <- (1 to 4).map(_ =>
             DenseVector.tabulate(n)(i => random.scalaRandom.nextGaussian() * math.sqrt(cov(i, i)) + mean(i))
           )) {
        breezeMvn.logPdf(v) should be(mvn.logpdf(v) +- 1e-5)
      }
    }
  }

  describe("A nD Multivariate normal") {
    it("should give the same logpdf values as breeze Gaussian with the same parameters to test numerical stability") {
      val n = 100
      val variance = 0.01
      val mean = DenseVector.zeros[Double](n)
      val cov = variance * DenseMatrix.eye[Double](n)
      for (x <- 1 until cov.rows) {
        cov(x, x - 1) = 0.002
        cov(x - 1, x) = 0.002
      }
      cov(-1, 0) = 0.002
      cov(0, -1) = 0.002
      val mvn = new MultivariateNormalDistribution(mean, cov)
      val breezeMvn = breeze.stats.distributions.MultivariateGaussian.apply(mean, cov)
      breezeMvn.logPdf(DenseVector.zeros[Double](n)) should be(mvn.logpdf(DenseVector.zeros[Double](n)) +- 1e-5)
    }
  }

  describe("A 3D Multivariate normal") {
    val mu = DenseVector(2.0, 1.0, 3.0)
    val X = DenseMatrix.rand[Double](3, 3, random.breezeRandBasis.uniform)
    val cov = X.t * X

    val mvn = new MultivariateNormalDistribution(mu, cov)

    it("yields the right mean and covariance matrix when we sample from the data") {
      val samples = for (i <- 0 until 3000) yield mvn.sample()
      val estimatedMVN = MultivariateNormalDistribution.estimateFromData(samples)
      for (i <- 0 until mu.length) {
        mu(i) should be(estimatedMVN.mean(i) +- 0.15)
      }
      for (i <- 0 until cov.rows; j <- 0 until cov.cols) {
        cov(i, j) should be(estimatedMVN.cov(i, j) +- 0.15)
      }
    }

    it("can reconstruct its covariance matrix from the computed principal components") {
      val (pcVecs, sigma2s) = mvn.principalComponents.unzip
      val uMat = DenseMatrix.zeros[Double](mvn.dim, mvn.dim)
      val sigma2Vec = DenseVector.zeros[Double](mvn.dim)
      for (i <- 0 until mvn.dim) {
        uMat(::, i) := pcVecs(i)
        sigma2Vec(i) = sigma2s(i)
      }
      val covRec = uMat * breeze.linalg.diag(sigma2Vec) * uMat.t
      for (i <- 0 until mvn.dim; j <- 0 until mvn.dim) {
        covRec(i, j) should be(mvn.cov(i, j) +- 1e-10)
      }
    }

    it("Can be constructed from the principal components") {

      val mvn2 = MultivariateNormalDistribution(mvn.mean, mvn.principalComponents)
      mvn.mean should equal(mvn2.mean)
      breeze.linalg.sum((mvn.cov - mvn2.cov).map(Math.abs)) should be < 1e-5
    }

  }

  describe("An MultivariateNormalDistribution") {
    it("returns the same principal components it was constructed with") {
      val axes =
        List(DenseVector[Double](1.0, 0.0, 0.0), DenseVector[Double](0.0, 1.0, 0.0), DenseVector[Double](0.0, 0.0, 1.0))
      // these are knowingly not sorted
      val variances = List(1.0, 4.0, 3.0)
      val data = axes zip variances
      val n = MultivariateNormalDistribution(DenseVector[Double](0.0, 0.0, 0.0), data)
      // to compare however, we must ensure that both are sorted
      n.principalComponents should equal(data.sortBy(x => x._2).reverse)
    }
  }

  describe("The mahalanobis distance") {
    it("is 0 if the mean is used") {
      val mu = DenseVector(2.0, 1.0)
      val cov = DenseMatrix.create[Double](2, 2, Array(1.0, 0.0, 0.0, 1.0))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      mvn.mahalanobisDistance(mu) should be(0.0 +- 1e-10)
    }

    it("yields the same as the squared norm if mean 0 and identity cov is used") {
      val mu = DenseVector(0.0, 0.0)
      val cov = DenseMatrix.create[Double](2, 2, Array(1.0, 0.0, 0.0, 1.0))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      val x = DenseVector(3.0, 7.0)
      mvn.mahalanobisDistance(x) should be(breeze.linalg.norm(x, 2))
    }

    it("increases with distance from the mean") {
      val mu = DenseVector(3.0, 0.0)
      val cov = DenseMatrix.create[Double](2, 2, Array(1.0, 0.0, 0.0, 1.0))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      for (i <- 1 until 10) {
        mvn.mahalanobisDistance(mu + DenseVector(1.0, i.toDouble)) should be > mvn.mahalanobisDistance(
          mu + DenseVector(1.0, (i - 1).toDouble)
        )
        mvn.mahalanobisDistance(mu + DenseVector(i.toDouble, 1.0)) should be > mvn.mahalanobisDistance(
          mu + DenseVector((i - 1).toDouble, 1.0)
        )
      }
    }
    it("gives value 2 for a sample with 2 stddev") {
      val variance = 5.0
      val stddev = math.sqrt(variance)
      val mvn = new MultivariateNormalDistribution(DenseVector(0.0), DenseMatrix.create[Double](1, 1, Array(variance)))
      mvn.mahalanobisDistance(DenseVector(stddev)) should be(1.0 +- 1e-5)
    }
  }

}
