/*
 * Copyright 2017 University of Basel, Graphics and Vision Research Group
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

import scalismo.ScalismoTestSuite
import scalismo.sampling.evaluators.GaussianEvaluator
import scalismo.sampling.proposals.MixtureProposal.{SymmetricProposalGenerator, SymmetricProposalGeneratorWithTransition}
import scalismo.sampling.{ProposalGenerator, SymmetricTransitionRatio, TransitionProbability, proposals}
import scalismo.utils.Random

class MixtureTests extends ScalismoTestSuite {

  describe("A MixtureProposal") {
    implicit val rnd = Random(89068)

    val gaussianProposal = new ProposalGenerator[Double] with TransitionProbability[Double] with SymmetricTransitionRatio[Double] {
      val sdev = 0.5

      /** draw a sample from this proposal distribution, may depend on current state */
      override def propose(current: Double): Double = rnd.scalaRandom.nextGaussian() * sdev + current

      /** rate of transition from to (log value) */
      override def logTransitionProbability(from: Double, to: Double): Double = GaussianEvaluator.logDensity(to, from, sdev)
    }

    val plainProposal = new ProposalGenerator[Double] {
      override def propose(current: Double): Double = current
    }

    val symProposal = new ProposalGenerator[Double] with SymmetricTransitionRatio[Double] {
      override def propose(current: Double): Double = current
    }

    val transProposal = new ProposalGenerator[Double] with TransitionProbability[Double] {
      override def propose(current: Double): Double = current
      override def logTransitionProbability(from: Double, to: Double): Double = 0.0
    }

    val symTransProposal = new ProposalGenerator[Double] with TransitionProbability[Double] with SymmetricTransitionRatio[Double] {
      override def propose(current: Double): Double = current
      override def logTransitionProbability(from: Double, to: Double): Double = 0.0
    }

    it("can be constructed from plain proposals") {
      val mixture = MixtureProposal.fromProposals((0.25, plainProposal), (0.75, plainProposal))
      mixture.generators should contain theSameElementsAs Seq(plainProposal, plainProposal)
      mixture.mixtureFactors shouldBe IndexedSeq(0.25, 0.75)
    }

    it("normalizes mixture coefficients") {
      val mixture = MixtureProposal.fromProposals((1.0, plainProposal), (2.0, plainProposal))
      mixture.mixtureFactors shouldBe IndexedSeq(1.0 / 3.0, 2.0 / 3.0)
    }

    it("calculates a correct mixed transition ratio") {
      val mixture = MixtureProposal.fromProposalsWithTransition((0.25, gaussianProposal), (0.75, gaussianProposal))
      mixture.logTransitionProbability(0.0, 1.0) shouldBe GaussianEvaluator.logDensity(1.0, 0.0, 0.5)
    }

    it("preserves symmetry of the transition probability for symmetric mixtures") {
      val mixture = MixtureProposal.fromSymmetricProposalsWithTransition((0.25, gaussianProposal), (0.75, gaussianProposal))
      mixture.logTransitionProbability(0.0, 1.0) shouldBe mixture.logTransitionProbability(1.0, 0.0)
    }

    describe("constructed by the implicit construction language") {
      import MixtureProposal.implicits._

      it("is a plain ProposalGenerator for plain proposals") {
        val mixture: ProposalGenerator[Double] = MixtureProposal(0.25 *: plainProposal + plainProposal * 0.75)
      }

      it("preserves symmetry of proposals") {
        val mixture: SymmetricProposalGenerator[Double] = MixtureProposal(0.25 *: symProposal + symProposal * 0.75)
      }

      it("preserves symmetry and transition probability of proposals") {
        val mixture: SymmetricProposalGeneratorWithTransition[Double] = MixtureProposal(0.25 *: symTransProposal + symTransProposal * 0.75)
      }

      it("discards symmetry if a proposal is not symmetric") {
        val mixture = MixtureProposal(0.25 *: symProposal + plainProposal * 0.75)
        mixture should not be a [SymmetricTransitionRatio[_]]
      }

      it("discards transition probability if a proposal does not provide it") {
        val mixture = MixtureProposal(0.25 *: symTransProposal + plainProposal * 0.75)
        mixture should not be a [TransitionProbability[_]]
      }

      it("properly interprets nested coefficients") {
        val mixture = MixtureProposal((plainProposal + plainProposal * 0.5) * 0.6 + 0.4 *: plainProposal)
        mixture.asInstanceOf[MixtureProposal[Double]].mixtureFactors shouldBe IndexedSeq(1.0/1.5 * 0.6, 0.5 / 1.5 * 0.6, 0.4)
      }
    }
  }
}
