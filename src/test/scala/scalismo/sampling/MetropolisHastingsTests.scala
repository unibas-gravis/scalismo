package scalismo.sampling

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.ScalismoTestSuite
import scalismo.sampling.algorithms.{MetropolisHastings, MetropolisHastingsWithPrefetching}
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.utils.Random

class MetropolisHastingsTests extends ScalismoTestSuite {

  implicit val rng: Random = scalismo.utils.Random(42)

  val gaussianProposal =
    new ProposalGenerator[Double] with TransitionProbability[Double] with SymmetricTransitionRatio[Double] {
      val sdev = 1.0

      /** draw a sample from this proposal distribution, may depend on current state */
      override def propose(current: Double): Double = rng.scalaRandom.nextGaussian() * sdev + current

      /** rate of transition from to (log value) */
      override def logTransitionProbability(from: Double, to: Double): Double =
        GaussianEvaluator.logDensity(to, from, sdev)
    }

  describe("The metropolis-hastings algorithm") {

    it("approximates the mean and covariance of a normal distribution") {
      val mean = 1.0
      val sdev = 3.5
      val evaluator = GaussianEvaluator(mean, sdev)

      val mh = MetropolisHastings(gaussianProposal, evaluator)
      val samples = mh.iterator(0.0).drop(100000).take(100000).toIndexedSeq
      val approximatedMean = samples.sum / samples.size
      val approximatedVariance =
        samples.map(sample => (sample - mean) * (sample - mean)).sum / samples.size

      approximatedMean should be(mean +- 1e-1)
      Math.sqrt(approximatedVariance) should be(sdev +- 5e-1)
    }
  }

  describe("The metropolis-hastings algorithm with prefetching") {

    it("approximates the mean and covariance of a normal distribution") {
      val mean = 1.0
      val sdev = 3.5
      val evaluator = GaussianEvaluator(mean, sdev)

      val mh = MetropolisHastingsWithPrefetching(gaussianProposal, evaluator)
      val samples = mh.iterator(0.0).drop(100000).take(1000000).toIndexedSeq
      val approximatedMean = samples.sum / samples.size
      val approximatedVariance =
        samples.map(sample => (sample - mean) * (sample - mean)).sum / samples.size

      approximatedMean should be(mean +- 1e-1)
      Math.sqrt(approximatedVariance) should be(sdev +- 5e-1)
    }
  }
}
