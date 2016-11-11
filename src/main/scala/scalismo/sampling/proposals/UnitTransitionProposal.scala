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

import scalismo.sampling.{ ProposalGenerator, UnitTransition }

import scala.language.implicitConversions

/** wraps an arbitrary proposal in a symmetric one with unit transition ratio and probabilities - use with care! */
class UnitTransitionProposal[A](proposal: ProposalGenerator[A]) extends ProposalGenerator[A] with UnitTransition[A] {
  /** draw a sample from this proposal distribution, may depend on current state */
  override def propose(current: A): A = proposal.propose(current)
}

object UnitTransitionProposal {
  def apply[A](proposal: ProposalGenerator[A]) = new UnitTransitionProposal[A](proposal)

  /** implicit attachment of "withUnitTransition" to each proposal */
  object implicits {
    implicit def toUTProposal[A](proposal: ProposalGenerator[A]): UTProposal[A] = new UTProposal[A](proposal)

    class UTProposal[A](proposal: ProposalGenerator[A]) {
      def withUnitTransition: ProposalGenerator[A] with UnitTransition[A] = UnitTransitionProposal(proposal)
    }
  }
}
