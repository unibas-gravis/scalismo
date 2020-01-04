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

import scalismo.sampling.{ProposalGenerator, TransitionProbability}

/**
 * Container for multiple ProposalGenerators stacked together, applied one after the other
 */
class CombinedProposal[A](val proposals: IndexedSeq[ProposalGenerator[A] with TransitionProbability[A]])
    extends ProposalGenerator[A]
    with TransitionProbability[A] {

  override def propose(current: A): A = {
    proposals.foldLeft(current) { (z: A, g: ProposalGenerator[A]) =>
      g.propose(z)
    }
  }

  /** transition from to */
  override def logTransitionProbability(from: A, to: A): Double = {
    proposals.foldLeft(0.0) { (z, g) =>
      z + g.logTransitionProbability(from, to)
    }
  }

  override def toString = {
    val s = new StringBuilder("CombinedProposal(")
    val firstNames = proposals.iterator.slice(0, 2).map(_.toString).mkString(",")
    s.append(firstNames)
    if (proposals.length > 2)
      s.append(",...")
    s.append(")")
    s.toString()
  }
}

object CombinedProposal {
  def apply[A](proposals: IndexedSeq[(ProposalGenerator[A] with TransitionProbability[A])]) =
    new CombinedProposal[A](proposals)
}
