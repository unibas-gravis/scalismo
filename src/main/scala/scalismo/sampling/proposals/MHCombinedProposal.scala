/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.sampling.proposals

import scalismo.sampling.{MHProposalGenerator, MHSample}

/**
 * Container for multiple ProposalGenerators stacked together, applied one after the other. It serves the same purpose
 * as CombinedProposal, but is made to work together with the MHSample. In particular, it creates a generatedBy String,
 * which references the name of all the proposals that are combined.
 */
class MHCombinedProposal[A](val proposals: Seq[MHProposalGenerator[A]]) extends MHProposalGenerator[A] {

  val identProposal = MHIdentityProposal.forType[A]

  override def propose(current: MHSample[A]): MHSample[A] = {

    val proposalWithGeneratedByString = proposals.foldLeft((current, "Combined(")) {
      (z: (MHSample[A], String), g: MHProposalGenerator[A]) =>
        val proposed = g.propose(z._1)
        (proposed, z._2 + proposed.generatedBy + ",")
    }
    proposalWithGeneratedByString._1.copy(generatedBy = proposalWithGeneratedByString._2.dropRight(1) + ")")

  }

  /** transition from to */
  override def logTransitionProbability(from: MHSample[A], to: MHSample[A]): Double = {
    proposals.foldLeft(0.0) { (z, g) =>
      z + g.logTransitionProbability(from, to)
    }
  }
}

object MHCombinedProposal {
  def apply[A](proposals: Seq[MHProposalGenerator[A]]) =
    new MHCombinedProposal[A](proposals)
}
