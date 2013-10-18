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

}