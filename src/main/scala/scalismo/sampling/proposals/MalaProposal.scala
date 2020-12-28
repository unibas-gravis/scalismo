package scalismo.sampling.proposals

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.sampling.{
  DistributionEvaluator,
  GradientEvaluator,
  ProposalGenerator,
  SampleLens,
  TransitionProbability
}
import scalismo.statisticalmodel.MultivariateNormalDistribution

/**
 * An implmentation of the Langevin Monte carlo as a proposal. Used with the
 * Metropolis-Hastings algorithm, it leads to the Metropolis-Adjusted Langevin Algorithm (MALA)
 * as described [here](https://en.wikipedia.org/wiki/Metropolis-adjusted_Langevin_algorithm).
 *
 * @param evaluator The probability density function to sample from
 * @param tau The time step to take in each iteration
 * @param sampleLens: A lense, which determines which part of the sample to consider for this proposal
 *
 */
case class MalaProposal[A](evaluator: DistributionEvaluator[A] with GradientEvaluator[A],
                           tau: Double,
                           sampleLens: SampleLens[A])(implicit rng: scalismo.utils.Random)
    extends ProposalGenerator[A]
    with TransitionProbability[A] {

  private val normalDist = MultivariateNormalDistribution(
    mean = DenseVector.zeros[Double](sampleLens.numberOfParameters),
    cov = DenseMatrix.eye[Double](sampleLens.numberOfParameters)
  )

  override def propose(sample: A): A = {

    val gradient = sampleLens.getAsVector(evaluator.gradient(sample))
    val normalSample = normalDist.sample() * Math.sqrt(2.0 * tau)
    val newParameters = sampleLens.getAsVector(sample) + gradient * tau + normalSample
    sampleLens.setFromVector(sample, newParameters, Some(s"GradientUpdatePropsosal ($tau"))
  }

  override def logTransitionProbability(from: A, to: A): Double = {
    val g = sampleLens.getAsVector(evaluator.gradient(from))
    Math.exp(
      breeze.linalg
        .norm(sampleLens.getAsVector(to) - sampleLens.getAsVector(from) - g * tau, 2) * (-1.0 / (4 * tau))
    )
  }
}
