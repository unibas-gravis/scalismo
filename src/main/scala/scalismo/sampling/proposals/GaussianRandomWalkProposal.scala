package scalismo.sampling.proposals

import breeze.linalg.DenseVector
import scalismo.common.Vectorizer
import scalismo.sampling.{MHProposalGenerator, MHSample, ProposalGenerator, SampleLens, TransitionProbability}



/**
 * Classical Random Walk proposal, where the current state is perturbed using a step, which is generated
 * by an isotropic gaussian with the given standard deviation.
 */
class GaussianRandomWalkProposal(stddev: Double, val tag : String)(implicit
  rng: scalismo.utils.Random
) extends MHProposalGenerator[DenseVector[Double]] { self =>

  override def propose(sample: MHSample[DenseVector[Double]]): MHSample[DenseVector[Double]] = {
    val partAsVec: DenseVector[Double] = sample.parameters
    val perturbation: DenseVector[Double] = DenseVector.rand(partAsVec.length, rng.breezeRandBasis.gaussian) * stddev
    sample.copy(parameters = partAsVec + perturbation, generatedBy = tag)
  }

  override def logTransitionProbability(from: MHSample[DenseVector[Double]], to: MHSample[DenseVector[Double]]): Double = {

    val dim = from.parameters.length
    math.pow(2.0 * math.Pi, -dim / 2.0) - 0.5
  }

  def partial(range : Range) : GaussianRandomWalkProposal = {
    new GaussianRandomWalkProposal(stddev, tag) {
      override def propose(sample : MHSample[DenseVector[Double]]) : MHSample[DenseVector[Double]] = {
        val partialNew = self.propose(sample.copy(parameters = sample.parameters(range))).parameters
        val newFull = sample.parameters.copy
        newFull(range) := partialNew
        sample.copy(parameters = newFull, generatedBy = tag)
      }

      override def logTransitionProbability(from: MHSample[DenseVector[Double]], to: MHSample[DenseVector[Double]]): Double = {
        self.logTransitionProbability(from.copy(parameters = from.parameters(range)), to.copy(parameters = to.parameters(range)))
      }
    }
  }


}

class MHIdentProposal[A]() extends MHProposalGenerator[A] {


  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: MHSample[A], to: MHSample[A]): Double = 0

  /** draw a sample from this proposal distribution, may depend on current state */
  override def propose(current: MHSample[A]): MHSample[A] = current.copy(generatedBy = "ident")
}

object MHIdentProposal {
  def forType[A] = new MHIdentProposal[A]()
}