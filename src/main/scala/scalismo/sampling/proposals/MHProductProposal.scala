package scalismo.sampling.proposals

import scalismo.sampling.{MHProposalGenerator, MHSample, ProposalGenerator, TransitionProbability}

object MHProductProposal {
  def apply[A, B](a: MHProposalGenerator[A], b: MHProposalGenerator[B]): MHProductProposal2[A, B] =
    new MHProductProposal2(a, b)

  def apply[A, B, C](a: MHProposalGenerator[A],
                     b: MHProposalGenerator[B],
                     c: MHProposalGenerator[C]): MHProductProposal3[A, B, C] = new MHProductProposal3(a, b, c)

  def apply[A, B, C, D](a: MHProposalGenerator[A],
                        b: MHProposalGenerator[B],
                        c: MHProposalGenerator[C],
                        d: MHProposalGenerator[D]): MHProductProposal4[A, B, C, D] = new MHProductProposal4(a, b, c, d)
}

class MHProductProposal2[A, B](proposal1: MHProposalGenerator[A], proposal2: MHProposalGenerator[B])
    extends MHProposalGenerator[(A, B)] {
  self =>

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: MHSample[(A, B)], to: MHSample[(A, B)]): Double = {

    proposal1.logTransitionProbability(MHSample(from.parameters._1, from.generatedBy),
                                       MHSample(to.parameters._1, to.generatedBy)) *
      proposal2.logTransitionProbability(MHSample(from.parameters._2, from.generatedBy),
                                         MHSample(to.parameters._2, to.generatedBy))
  }

  /** draw a sample from this proposal distribution, may depend on current state */
  override def propose(current: MHSample[(A, B)]): MHSample[(A, B)] = {
    val sample1 = proposal1.propose(current.copy(parameters = current.parameters._1))
    val sample2 = proposal2.propose(current.copy(parameters = current.parameters._2))
    MHSample(
      (sample1.parameters, sample2.parameters),
      generatedBy = s"(${sample1.generatedBy}, ${sample2.generatedBy})"
    )
  }
}

class MHProductProposal3[A, B, C](proposal1: MHProposalGenerator[A],
                                  proposal2: MHProposalGenerator[B],
                                  proposal3: MHProposalGenerator[C])
    extends MHProposalGenerator[(A, B, C)] {
  self =>

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: MHSample[(A, B, C)], to: MHSample[(A, B, C)]): Double = {

    proposal1.logTransitionProbability(MHSample(from.parameters._1, from.generatedBy),
                                       MHSample(to.parameters._1, to.generatedBy)) *
      proposal2.logTransitionProbability(MHSample(from.parameters._2, from.generatedBy),
                                         MHSample(to.parameters._2, to.generatedBy)) *
      proposal3.logTransitionProbability(MHSample(from.parameters._3, from.generatedBy),
                                         MHSample(to.parameters._3, to.generatedBy))
  }

  /** draw a sample from this proposal distribution, may depend on current state */
  override def propose(current: MHSample[(A, B, C)]): MHSample[(A, B, C)] = {
    val sample1 = proposal1.propose(current.copy(parameters = current.parameters._1))
    val sample2 = proposal2.propose(current.copy(parameters = current.parameters._2))
    val sample3 = proposal3.propose(current.copy(parameters = current.parameters._3))
    MHSample(
      (sample1.parameters, sample2.parameters, sample3.parameters),
      generatedBy = s"(${sample1.generatedBy}, ${sample2.generatedBy}, ${sample3.generatedBy})"
    )
  }

}

class MHProductProposal4[A, B, C, D](proposal1: MHProposalGenerator[A],
                                     proposal2: MHProposalGenerator[B],
                                     proposal3: MHProposalGenerator[C],
                                     proposal4: MHProposalGenerator[D])
    extends MHProposalGenerator[(A, B, C, D)] {
  self =>

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: MHSample[(A, B, C, D)], to: MHSample[(A, B, C, D)]): Double = {

    proposal1.logTransitionProbability(MHSample(from.parameters._1, from.generatedBy),
                                       MHSample(to.parameters._1, to.generatedBy)) *
      proposal2.logTransitionProbability(MHSample(from.parameters._2, from.generatedBy),
                                         MHSample(to.parameters._2, to.generatedBy)) *
      proposal3.logTransitionProbability(MHSample(from.parameters._3, from.generatedBy),
                                         MHSample(to.parameters._3, to.generatedBy)) *
      proposal4.logTransitionProbability(MHSample(from.parameters._4, from.generatedBy),
                                         MHSample(to.parameters._4, to.generatedBy))
  }

  /** draw a sample from this proposal distribution, may depend on current state */
  override def propose(current: MHSample[(A, B, C, D)]): MHSample[(A, B, C, D)] = {
    val sample1 = proposal1.propose(current.copy(parameters = current.parameters._1))
    val sample2 = proposal2.propose(current.copy(parameters = current.parameters._2))
    val sample3 = proposal3.propose(current.copy(parameters = current.parameters._3))
    val sample4 = proposal4.propose(current.copy(parameters = current.parameters._4))
    MHSample(
      (sample1.parameters, sample2.parameters, sample3.parameters, sample4.parameters),
      generatedBy = s"(${sample1.generatedBy}, ${sample2.generatedBy}, ${sample3.generatedBy}, ${sample4.generatedBy})"
    )
  }

}
