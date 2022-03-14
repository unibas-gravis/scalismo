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
import scalismo.utils.Random

/**
 * Mixture proposal for the special case where the proposal is an MHProposal
 */
class MHMixtureProposal[A](proposals: IndexedSeq[(Double, MHProposalGenerator[A])])(implicit rnd: Random)
    extends MHProposalGenerator[A] {

  val generators: IndexedSeq[MHProposalGenerator[A]] = proposals.map(_._2)

  val mixtureFactors: IndexedSeq[Double] = {
    val f = proposals.map(_._1)
    val totalP = f.sum
    f.map(c => c / totalP)
  }

  // cumsum
  protected val p: IndexedSeq[Double] = mixtureFactors.scanLeft(0.0)((t, p) => t + p).tail
  // keep state: last active proposal, useful for printing only
  private var lastActive = 0

  override def propose(current: MHSample[A]): MHSample[A] = {
    val r = rnd.scalaRandom.nextDouble()
    val i = p.indexWhere(p => p >= r) // find first element larger than random, use l
    lastActive = i
    generators(i).propose(current)
  }

  override def logTransitionProbability(from: MHSample[A], to: MHSample[A]): Double = {
    val transitions = generators.map(g => g.logTransitionProbability(from, to))
    if (transitions.exists(_.isNaN))
      throw new Exception("NaN transition probability encountered!")
    if (transitions.exists(!_.isInfinite)) {
      val maxExpo = transitions.max
      val sum = mixtureFactors.zip(transitions).map { case (f, t) => f * math.exp(t - maxExpo) }.sum
      val fwd = math.log(sum) + maxExpo
      fwd
    } else
      Double.NegativeInfinity
  }

}

object MHMixtureProposal {
  def apply[A](proposals: (Double, MHProposalGenerator[A])*)(implicit rnd: Random): MHMixtureProposal[A] =
    new MHMixtureProposal[A](proposals.toIndexedSeq)
}
