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
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.loggers.{ AcceptRejectLogger, SilentLogger }
import scalismo.utils.Random

/** Metropolis Filter Proposal with no correction */
class MetropolisFilterProposal[A](
  val generator: ProposalGenerator[A] with TransitionRatio[A],
  val evaluator: DistributionEvaluator[A],
  val logger: AcceptRejectLogger[A])(implicit random: Random)
  extends ProposalGenerator[A] with TransitionProbability[A] {

  private val mH = MetropolisHastings(generator, evaluator)

  override def propose(current: A): A = mH.next(current, logger)

  /** rate of transition from to (log value) */
  override def logTransitionProbability(from: A, to: A): Double = {
    // not corrected here! filter should "multiply-in" the filtering distribution
    0.0
  }

  override def toString = s"MetropolisFilterProposal($generator,$evaluator)"
}

object MetropolisFilterProposal {
  def apply[A](
    generator: ProposalGenerator[A] with TransitionRatio[A],
    evaluator: DistributionEvaluator[A],
    logger: AcceptRejectLogger[A])(implicit random: Random) = new MetropolisFilterProposal[A](generator, evaluator, logger)

  def apply[A](
    generator: ProposalGenerator[A] with TransitionRatio[A],
    evaluator: DistributionEvaluator[A])(implicit random: Random) = new MetropolisFilterProposal[A](generator, evaluator, new SilentLogger[A])

}

/** Metropolis Filter Proposal corrected with transition probability */
class CorrectedMetropolisFilterProposal[A](
  val generator: ProposalGenerator[A] with TransitionRatio[A],
  val evaluator: DistributionEvaluator[A],
  val logger: AcceptRejectLogger[A])(implicit random: Random)
  extends ProposalGenerator[A] with TransitionProbability[A] {

  private val mH = MetropolisHastings(generator, evaluator)

  override def propose(current: A): A = mH.next(current, logger)

  override def logTransitionRatio(from: A, to: A): Double = {
    evaluator.logValue(to) - evaluator.logValue(from)
  }

  override def logTransitionProbability(from: A, to: A): Double = {
    evaluator.logValue(to)
  }

  override def toString = s"MetropolisHastingsFilterProposal($generator,$evaluator)"

}

object CorrectedMetropolisFilterProposal {
  def apply[A](
    generator: ProposalGenerator[A] with TransitionRatio[A],
    evaluator: DistributionEvaluator[A],
    logger: AcceptRejectLogger[A])(implicit random: Random) = new CorrectedMetropolisFilterProposal[A](generator, evaluator, logger)

  def apply[A](
    generator: ProposalGenerator[A] with TransitionRatio[A],
    evaluator: DistributionEvaluator[A])(implicit random: Random) = new CorrectedMetropolisFilterProposal[A](generator, evaluator, new SilentLogger[A])

}
