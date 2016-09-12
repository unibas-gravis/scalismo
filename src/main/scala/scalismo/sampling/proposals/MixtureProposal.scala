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

import scalismo.sampling.{ ProposalGenerator, SymmetricTransition, SymmetricTransitionRatio, TransitionProbability }
import scalismo.utils.Random

/** mixture of proposals: mixture distribution of multiple proposal distributions */
class MixtureProposal[A](proposals: IndexedSeq[(Double, ProposalGenerator[A])])(implicit rnd: Random)
    extends ProposalGenerator[A] {

  /** mixture components */
  val generators = proposals.map(_._2)

  val mixtureFactors = {
    val f = proposals.map(_._1)
    val totalP = f.sum
    f.map(c => c / totalP)
  }

  // cumsum
  protected val p = mixtureFactors.scanLeft(0.0)((t, p) => t + p).tail
  // keep state: last active proposal, useful for printing only
  private var lastActive = 0

  override def propose(current: A): A = {
    val r = rnd.scalaRandom.nextDouble()
    val i = p.indexWhere(p => p >= r) // find first element larger than random, use l
    lastActive = i
    generators(i).propose(current)
  }

  override def toString = generators(lastActive).toString
}

/** mixture with transition probabilities */
private class MixtureProposalWithTransition[A](proposals: IndexedSeq[(Double, ProposalGenerator[A] with TransitionProbability[A])])(implicit rnd: Random)
    extends MixtureProposal[A](proposals) with TransitionProbability[A] {

  override val generators = proposals.map(_._2)

  override def logTransitionProbability(from: A, to: A): Double = {
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

object MixtureProposal {
  /** generate a mixture proposal */
  def fromProposals[A](proposals: (Double, ProposalGenerator[A])*)(implicit rnd: Random): MixtureProposal[A] = new MixtureProposal[A](proposals.toIndexedSeq)

  /** mixture distribution of proposals with a transition probability */
  def fromProposalsWithTransition[A](proposals: (Double, ProposalGenerator[A] with TransitionProbability[A])*)(implicit rnd: Random): MixtureProposal[A] with TransitionProbability[A] = new MixtureProposalWithTransition[A](proposals.toIndexedSeq)

  /** mixture of symmetric proposals (mixture distribution) */
  def fromSymmetricProposals[A](proposals: (Double, ProposalGenerator[A] with SymmetricTransitionRatio[A])*)(implicit rnd: Random): MixtureProposal[A] with SymmetricTransitionRatio[A] = new MixtureProposal[A](proposals.toIndexedSeq) with SymmetricTransitionRatio[A]

  /** mixture of symmetric proposals (mixture distribution) with a transition probability */
  def fromSymmetricProposalsWithTransition[A](proposals: (Double, ProposalGenerator[A] with SymmetricTransition[A])*)(implicit rnd: Random): MixtureProposal[A] with SymmetricTransition[A] = new MixtureProposalWithTransition[A](proposals.toIndexedSeq) with SymmetricTransition[A]

  /** create a mixture proposal using the simplified operator syntax: 0.4 *: prop1 + 0.6 *: prop2 */
  def apply[A](builder: implicits.MixtureBuilder[A])(implicit rnd: Random): MixtureProposal[A] with TransitionProbability[A] = builder.toMixtureProposal

  /** implicit conversions for simple building: MixtureProposal(0.5 *: prop1 + 0.25 *: prop2 + 0.25 *: prop3) - only for ProposalGenerator[A] with TransitionProbability[A] */
  object implicits {

    import scala.language.implicitConversions

    implicit def toComponent[A](proposal: ProposalGenerator[A] with TransitionProbability[A]): MixtureComponent[A] = new MixtureComponent[A](proposal, 1)

    implicit def toBuilder[A](comp: MixtureComponent[A]): MixtureBuilder[A] = new MixtureBuilder[A](comp)

    implicit def toProposal[A](builder: MixtureBuilder[A])(implicit rnd: Random): MixtureProposal[A] with TransitionProbability[A] = builder.toMixtureProposal

    case class MixtureComponent[A](proposal: ProposalGenerator[A] with TransitionProbability[A], factor: Double = 1.0) {
      def *:(f: Float) = MixtureComponent(proposal, factor * f)

      def *:(f: Double) = MixtureComponent(proposal, factor * f)

      def :*(f: Float) = MixtureComponent(proposal, factor * f)

      def :*(f: Double) = MixtureComponent(proposal, factor * f)

      def +(other: MixtureComponent[A]) = new MixtureBuilder[A](this, other)

      def +(other: ProposalGenerator[A] with TransitionProbability[A]) = new MixtureBuilder[A](this, other)

      def tupled = (factor, proposal)
    }

    class MixtureBuilder[A](comps: MixtureComponent[A]*) {
      def +(other: MixtureComponent[A]): MixtureBuilder[A] = new MixtureBuilder[A](comps :+ other: _*)

      def +(other: ProposalGenerator[A] with TransitionProbability[A]): MixtureBuilder[A] = this + MixtureComponent(other)

      def toMixtureProposal(implicit rnd: Random): MixtureProposal[A] with TransitionProbability[A] = MixtureProposal.fromProposalsWithTransition(comps.map(_.tupled): _*)
    }
  }
}
