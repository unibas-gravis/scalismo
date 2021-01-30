package scalismo.sampling.proposals

import scalismo.sampling.{ProposalGenerator, SampleLens, TransitionProbability}

/**
 * Adapter to obtain a full proposal from a proposal generator that is only defined on a part of the
 * parameter space. The part on which the proposal generator is defined is defined by the given sampleLens
 *
 * @param proposal A proposal generator that generates a new proposal for a given part
 * @param lens A lense that focuses on the desired part of the parameter vector
 */
class PartialProposalAdapter[A, Part](proposal: ProposalGenerator[Part] with TransitionProbability[Part],
                                      lens: SampleLens[A, Part])
    extends ProposalGenerator[A]
    with TransitionProbability[A] {

  override def propose(current: A): A = {
    val partialProposal = proposal.propose(lens.get(current))
    lens.replace(current, partialProposal)
  }

  override def logTransitionProbability(from: A, to: A): Double = {
    proposal.logTransitionProbability(lens.get(from), lens.get(to))
  }
}
