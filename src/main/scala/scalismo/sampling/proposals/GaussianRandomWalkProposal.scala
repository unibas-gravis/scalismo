package scalismo.sampling.proposals

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.sampling.{ProposalGenerator, SampleLens, TransitionProbability}
import scalismo.statisticalmodel.MultivariateNormalDistribution

/**
 * Classical Random Walk proposal, where the current state is perturbed using a step, which is generated
 * by an isotropic gaussian with the given standard deviation.
 */
case class GaussianRandomWalkProposal[A](stddev: Double, sampleLens: SampleLens[A])(implicit rng: scalismo.utils.Random)
    extends ProposalGenerator[A]
    with TransitionProbability[A] {

  private val normalDist = new MultivariateNormalDistribution(
    DenseVector.zeros(sampleLens.numberOfParameters),
    DenseMatrix.eye[Double](sampleLens.numberOfParameters) * stddev * stddev
  )

  override def propose(sample: A): A = {
    val perturbation = normalDist.sample()

    val newParameters = sampleLens.getAsVector(sample) + normalDist.sample()
    sampleLens.setFromVector(sample, newParameters, Some(s"GaussianRandomWalkProposal ($stddev"))
  }

  override def logTransitionProbability(from: A, to: A): Double = {
    val residual = sampleLens.getAsVector(to) - sampleLens.getAsVector(from)
    normalDist.logpdf(residual)
  }
}
