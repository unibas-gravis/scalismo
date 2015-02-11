package scalismo.statisticalmodel

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Matchers, FunSpec}
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scalismo.geometry._

class MultivariateNormalDistributionTests extends FunSpec with Matchers {
  describe("A 1D Multivariate normal") {
    it("should give the same pdf values as breeze Gaussian with the same parameters") {
      val mvn = new MultivariateNormalDistribution(DenseVector(2f), DenseMatrix(3f))
      val n = breeze.stats.distributions.Gaussian(2, Math.sqrt(3f))
      for (pt <- Seq(0, 0.1, -2, 3.1)) {
        n.pdf(pt) should be(mvn.pdf(DenseVector(pt.toFloat)) +- 1e-5)
      }
    }

  }


  describe("A 3D Multivariate normal") {
    val mu = DenseVector(2f, 1.0f, 3f)
    val X = DenseMatrix.rand(3, 3).map(_.toFloat)
    val cov = X.t * X

    val mvn = new MultivariateNormalDistribution(mu, cov)

    it("yields the right mean and covariance matrix when we sample from the data") {
      val samples = for (i <- 0 until 2000) yield mvn.drawSample()
      val estimatedMVN = MultivariateNormalDistribution.estimateFromData(samples)
      for (i <- 0 until mu.length) {
        mu(i) should be(estimatedMVN.mean(i) +- 0.1f)
      }
      for (i <- 0 until cov.rows; j <- 0 until cov.cols) {
        cov(i,j) should be(estimatedMVN.cov(i,j) +- 0.15f)
      }
    }

    it("can reconstruct its covariance matrix from the principal components") {
      val (pcVecs, sigma2s) = mvn.principalComponents.unzip
      val uMat = DenseMatrix.zeros[Float](mvn.dim, mvn.dim)
      val sigma2Vec = DenseVector.zeros[Float](mvn.dim)
      for (i <- 0 until mvn.dim) {
        uMat(::, i) := pcVecs(i)
        sigma2Vec(i) = sigma2s(i).toFloat
      }
      val covRec = uMat * breeze.linalg.diag(sigma2Vec) * uMat.t
      for (i <- 0 until mvn.dim; j <- 0 until mvn.dim) {
        covRec(i,j) should be (mvn.cov(i,j) +- 1e-5f)
      }
    }
  }

  describe("An NDimensionalNormalDistribution") {
    it("returns the same principal components it was constructed with") {
      val axes = List(Vector(1, 0, 0), Vector(0, 1, 0), Vector(0, 0, 1))
      // these are knowingly not sorted
      val variances = List(1.0f, 4.0f, 3.0f)
      val data = axes zip variances
      val n = NDimensionalNormalDistribution(Vector(0, 0, 0), data)
      // to compare however, we must ensure that both are sorted
      n.principalComponents should equal(data.sortBy(x => x._2).reverse)
    }
  }

  describe("The mahalanobis distance") {
    it("is 0 if the mean is used") {
      val mu = DenseVector(2f, 1.0f)
      val cov = DenseMatrix.create[Float](2,2, Array(1, 0, 0, 1))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      mvn.mahalanobisDistance(mu) should be(0.0 +- 1e-5)
    }

    it("yields the same as the squared norm if mean 0 and identity cov is used") {
      val mu = DenseVector(0f, 0f)
      val cov = DenseMatrix.create[Float](2,2, Array(1, 0, 0, 1))
      val mvn = new MultivariateNormalDistribution(mu, cov)
      val x = DenseVector(3f, 7f)
      mvn.mahalanobisDistance(x) should be(breeze.linalg.norm(x, 2))
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
      mvn.mahalanobisDistance(DenseVector(math.sqrt(variance).toFloat)) should be(1.0 +- 1e-5)
    }
  }


  
  

}