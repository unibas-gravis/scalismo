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

import scalismo.sampling._
import scalismo.utils.Random

/** mixture of proposals: mixture distribution of multiple proposal distributions */
class MixtureProposal[A](proposals: IndexedSeq[(Double, ProposalGenerator[A])])(implicit rnd: Random)
    extends ProposalGenerator[A] {

  /** mixture components */
  val generators: IndexedSeq[ProposalGenerator[A]] = proposals.map(_._2)

  val mixtureFactors: IndexedSeq[Double] = {
    val f = proposals.map(_._1)
    val totalP = f.sum
    f.map(c => c / totalP)
  }

  // cumsum
  protected val p: IndexedSeq[Double] = mixtureFactors.scanLeft(0.0)((t, p) => t + p).tail
  // keep state: last active proposal, useful for printing only
  private var lastActive = 0

  override def propose(current: A): A = {
    val r = rnd.scalaRandom.nextDouble()
    val i = p.indexWhere(p => p >= r) // find first element larger than random, use l
    lastActive = i
    generators(i).propose(current)
  }

  override def toString: String = generators(lastActive).toString
}

/** mixture with transition probabilities */
private class MixtureProposalWithTransition[A](
  proposals: IndexedSeq[(Double, ProposalGenerator[A] with TransitionProbability[A])]
)(implicit rnd: Random)
    extends MixtureProposal[A](proposals)
    with TransitionProbability[A] {

  override val generators: IndexedSeq[ProposalGenerator[A] with TransitionProbability[A]] = proposals.map(_._2)

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
  def fromProposals[A](proposals: (Double, ProposalGenerator[A])*)(implicit rnd: Random): MixtureProposal[A] =
    new MixtureProposal[A](proposals.toIndexedSeq)

  /** mixture distribution of proposals with a transition probability */
  def fromProposalsWithTransition[A](proposals: (Double, ProposalGenerator[A] with TransitionProbability[A])*)(
    implicit
    rnd: Random
  ): MixtureProposal[A] with TransitionProbability[A] = new MixtureProposalWithTransition[A](proposals.toIndexedSeq)

  /** mixture of symmetric proposals (mixture distribution) */
  def fromSymmetricProposals[A](
    proposals: (Double, ProposalGenerator[A] with SymmetricTransitionRatio[A])*
  )(implicit rnd: Random): MixtureProposal[A] with SymmetricTransitionRatio[A] =
    new MixtureProposal[A](proposals.toIndexedSeq) with SymmetricTransitionRatio[A]

  /** mixture of symmetric proposals (mixture distribution) with a transition probability */
  def fromSymmetricProposalsWithTransition[A](
    proposals: (Double, ProposalGenerator[A] with SymmetricTransitionRatio[A] with TransitionProbability[A])*
  )(implicit rnd: Random): MixtureProposal[A] with SymmetricTransitionRatio[A] with TransitionProbability[A] =
    new MixtureProposalWithTransition[A](proposals.toIndexedSeq) with SymmetricTransitionRatio[A]

  /** create a mixture proposal from a sequence of components - use together with implicits._ for the simplified operator syntax: MixtureProposal(0.4 *: prop1 + 0.6 *: prop2) */
  def apply[A](components: Seq[(Double, A)])(implicit builder: CreateMixture[A], rnd: Random): A =
    builder.create(components)

  type SymmetricProposalGenerator[A] = ProposalGenerator[A] with SymmetricTransitionRatio[A]
  type SymmetricProposalGeneratorWithTransition[A] =
    ProposalGenerator[A] with SymmetricTransitionRatio[A] with TransitionProbability[A]
  type ProposalGeneratorWithTransition[A] = ProposalGenerator[A] with TransitionProbability[A]
  type ProposalGeneratorWithTransitionRatio[A] = ProposalGenerator[A] with TransitionRatio[A]

  private def normalizeCoefficients[A](components: Seq[(Double, A)]): Seq[(Double, A)] = {
    val total = components.map { _._1 }.sum
    components.map { case (coeff, gen) => (coeff / total, gen) }
  }

  trait CreateMixture[A] {
    def create(components: Seq[(Double, A)])(implicit rnd: Random): A
  }

  object CreateMixture {
    implicit def propBuilder[A]: CreateMixture[ProposalGenerator[A]] = new CreateMixture[ProposalGenerator[A]] {
      override def create(
        components: Seq[(Double, ProposalGenerator[A])]
      )(implicit rnd: Random): ProposalGenerator[A] = {
        MixtureProposal.fromProposals(normalizeCoefficients(components): _*)
      }
    }

    implicit def symBuilder[A]: CreateMixture[SymmetricProposalGenerator[A]] =
      new CreateMixture[SymmetricProposalGenerator[A]] {
        override def create(
          components: Seq[(Double, SymmetricProposalGenerator[A])]
        )(implicit rnd: Random): SymmetricProposalGenerator[A] = {
          MixtureProposal.fromSymmetricProposals(normalizeCoefficients(components): _*)
        }
      }

    implicit def transBuilder[A]: CreateMixture[ProposalGeneratorWithTransition[A]] =
      new CreateMixture[ProposalGeneratorWithTransition[A]] {
        override def create(
          components: Seq[(Double, ProposalGeneratorWithTransition[A])]
        )(implicit rnd: Random): ProposalGeneratorWithTransition[A] = {
          MixtureProposal.fromProposalsWithTransition(normalizeCoefficients(components): _*)
        }
      }

    implicit def symTransBuilder[A]: CreateMixture[SymmetricProposalGeneratorWithTransition[A]] =
      new CreateMixture[SymmetricProposalGeneratorWithTransition[A]] {
        override def create(
          components: Seq[(Double, SymmetricProposalGeneratorWithTransition[A])]
        )(implicit rnd: Random): SymmetricProposalGeneratorWithTransition[A] = {
          MixtureProposal.fromSymmetricProposalsWithTransition(normalizeCoefficients(components): _*)
        }
      }
  }

  /** implicit conversions for simple building: MixtureProposal(0.5 *: prop1 + 0.25 *: prop2 + 0.25 *: prop3) - handles all symmetry and transition traits */
  object implicits {

    import scala.language.implicitConversions

    implicit def proposal2Builder[A](comp: ProposalGenerator[A]): MixtureBuilder[A] =
      new MixtureBuilder[A](IndexedSeq((1.0, comp)))
    implicit def proposal2BuilderSym[A](comp: SymmetricProposalGenerator[A]): MixtureBuilderSym[A] =
      new MixtureBuilderSym[A](IndexedSeq((1.0, comp)))
    implicit def proposal2BuilderTrans[A](comp: ProposalGeneratorWithTransition[A]): MixtureBuilderTrans[A] =
      new MixtureBuilderTrans[A](IndexedSeq((1.0, comp)))
    implicit def proposal2BuilderSymTrans[A](
      comp: SymmetricProposalGeneratorWithTransition[A]
    ): MixtureBuilderSymTrans[A] = new MixtureBuilderSymTrans[A](IndexedSeq((1.0, comp)))

    trait MixtureComponentSeq[A] extends Seq[(Double, A)] {
      def components: Seq[(Double, A)]
      override def length: Int = components.length
      override def apply(idx: Int): (Double, A) = components(idx)
      override def iterator: Iterator[(Double, A)] = components.iterator
    }

    implicit class MixtureBuilder[A](override val components: Seq[(Double, ProposalGenerator[A])])
        extends MixtureComponentSeq[ProposalGenerator[A]] {
      def +(other: MixtureBuilder[A]) = new MixtureBuilder[A](components ++ other)

      def *(f: Double) =
        new MixtureBuilder[A](normalizeCoefficients(components).map { case (coeff, gen) => (coeff * f, gen) })
      def *:(f: Double): MixtureBuilder[A] = this * f
    }

    implicit class MixtureBuilderSym[A](override val components: Seq[(Double, SymmetricProposalGenerator[A])])
        extends MixtureComponentSeq[SymmetricProposalGenerator[A]] {
      def +(other: MixtureBuilderSym[A]): Seq[(Double, SymmetricProposalGenerator[A])] =
        new MixtureBuilderSym[A](components ++ other)
      def +(other: MixtureBuilder[A]): Seq[(Double, ProposalGenerator[A])] = new MixtureBuilder[A](components ++ other)

      def *(f: Double) =
        new MixtureBuilderSym[A](normalizeCoefficients(components).map { case (coeff, gen) => (coeff * f, gen) })
      def *:(f: Double): MixtureBuilderSym[A] = this * f
    }

    implicit class MixtureBuilderTrans[A](override val components: Seq[(Double, ProposalGeneratorWithTransition[A])])
        extends MixtureComponentSeq[ProposalGeneratorWithTransition[A]] {
      def +(other: MixtureBuilderTrans[A]): Seq[(Double, ProposalGeneratorWithTransition[A])] =
        new MixtureBuilderTrans[A](components ++ other)
      def +(other: MixtureBuilder[A]): Seq[(Double, ProposalGenerator[A])] = new MixtureBuilder[A](components ++ other)

      def *(f: Double) =
        new MixtureBuilderTrans[A](normalizeCoefficients(components).map { case (coeff, gen) => (coeff * f, gen) })
      def *:(f: Double): MixtureBuilderTrans[A] = this * f
    }

    implicit class MixtureBuilderSymTrans[A](
      override val components: Seq[(Double, SymmetricProposalGeneratorWithTransition[A])]
    ) extends MixtureComponentSeq[SymmetricProposalGeneratorWithTransition[A]] {
      def +(other: MixtureBuilderSymTrans[A]): Seq[(Double, SymmetricProposalGeneratorWithTransition[A])] =
        new MixtureBuilderSymTrans[A](components ++ other)
      def +(other: MixtureBuilderSym[A]): Seq[(Double, SymmetricProposalGenerator[A])] =
        new MixtureBuilderSym[A](components ++ other)
      def +(other: MixtureBuilderTrans[A]): Seq[(Double, ProposalGeneratorWithTransition[A])] =
        new MixtureBuilderTrans[A](components ++ other)
      def +(other: MixtureBuilder[A]): Seq[(Double, ProposalGenerator[A])] = new MixtureBuilder[A](components ++ other)

      def *(f: Double) =
        new MixtureBuilderSymTrans[A](normalizeCoefficients(components).map { case (coeff, gen) => (coeff * f, gen) })
      def *:(f: Double): MixtureBuilderSymTrans[A] = this * f
    }
  }
}
