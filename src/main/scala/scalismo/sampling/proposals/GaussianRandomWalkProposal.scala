package scalismo.sampling.proposals

import breeze.linalg.{DenseVector}
import scalismo.sampling.{ProposalGenerator, SampleLens, TransitionProbability}

/**
 * Classical Random Walk proposal, where the current state is perturbed using a step, which is generated
 * by an isotropic gaussian with the given standard deviation.
 */
case class GaussianRandomWalkProposal[A](stddev: Double, lens: SampleLens[A, DenseVector[Double]])(
  implicit rng: scalismo.utils.Random
) extends ProposalGenerator[A]
    with TransitionProbability[A] {

  override def propose(sample: A): A = {
    val part = lens.get(sample)
    val perturbation = DenseVector.rand(part.length, rng.breezeRandBasis.gaussian) * stddev
    lens.replace(sample, part + perturbation, generatedBy = Some(s"GaussianRandomWalkProposal(stddev=$stddev)"))
  }

  override def logTransitionProbability(from: A, to: A): Double = {
    val dim = lens.get(from).length
    math.pow(2.0 * math.Pi, -dim / 2.0) - 0.5
  }
}
