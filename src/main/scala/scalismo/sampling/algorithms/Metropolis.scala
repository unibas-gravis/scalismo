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
package scalismo.sampling.algorithms

import scalismo.sampling._
import scalismo.sampling.loggers.{AcceptRejectLogger, SilentLogger}
import scalismo.utils.Random

import scala.math.exp

/**
 * Metropolis algorithm (MCMC), provides samples from the evaluator distribution by drawing from generator and stochastic accept/reject decisions
 * generator needs to be symmetric
 */
class Metropolis[A] protected (val generator: ProposalGenerator[A] with SymmetricTransitionRatio[A],
                               val evaluator: DistributionEvaluator[A])(implicit val random: Random)
    extends MarkovChain[A] {

  /** internal fallback logger which does nothing */
  private lazy val silentLogger: AcceptRejectLogger[A] = new SilentLogger[A]()

  /** start a logged iterator */
  def iterator(start: A, logger: AcceptRejectLogger[A]): Iterator[A] = Iterator.iterate(start) { next(_, logger) }

  // next sample
  def next(current: A, logger: AcceptRejectLogger[A]): A = {
    // reference p value
    val currentP = evaluator.logValue(current)
    // proposal
    val proposal = generator.propose(current)
    val proposalP = evaluator.logValue(proposal)
    // acceptance probability
    val a = proposalP - currentP
    // accept or reject
    if (a > 0.0 || random.scalaRandom.nextDouble() < exp(a)) {
      logger.accept(current, proposal, generator, evaluator)
      proposal
    } else {
      logger.reject(current, proposal, generator, evaluator)
      current
    }
  }

  override def next(current: A): A = next(current, silentLogger)
}

object Metropolis {

  /** create a Metropolis MCMC chain, needs a symmetric proposal distribution, with logger attached */
  def apply[A](generator: ProposalGenerator[A] with SymmetricTransitionRatio[A], evaluator: DistributionEvaluator[A])(
    implicit random: Random
  ) = new Metropolis[A](generator, evaluator)
}

/** Metropolis-Hastings algorithm - generates random samples from a target distribution using only samples from a proposal distribution */
class MetropolisHastings[A] protected (val generator: ProposalGenerator[A] with TransitionRatio[A],
                                       val evaluator: DistributionEvaluator[A])(implicit val random: Random)
    extends MarkovChain[A] {

  private lazy val silentLogger = new SilentLogger[A]()

  /** start a logged iterator */
  def iterator(start: A, logger: AcceptRejectLogger[A]): Iterator[A] = Iterator.iterate(start) { next(_, logger) }

  // next sample
  def next(current: A, logger: AcceptRejectLogger[A]): A = {
    // reference p value
    val currentP = evaluator.logValue(current)
    // proposal
    val proposal = generator.propose(current)
    val proposalP = evaluator.logValue(proposal)
    // transition ratio
    val t = generator.logTransitionRatio(current, proposal)
    // acceptance probability
    val a = proposalP - currentP - t

    // accept or reject
    if (a > 0.0 || random.scalaRandom.nextDouble() < exp(a)) {
      logger.accept(current, proposal, generator, evaluator)
      proposal
    } else {
      logger.reject(current, proposal, generator, evaluator)
      current
    }
  }

  /** next sample in chain */
  override def next(current: A): A = next(current, silentLogger)
}

object MetropolisHastings {
  def apply[A](generator: ProposalGenerator[A] with TransitionRatio[A], evaluator: DistributionEvaluator[A])(
    implicit random: Random
  ) = new MetropolisHastings[A](generator, evaluator)
}
