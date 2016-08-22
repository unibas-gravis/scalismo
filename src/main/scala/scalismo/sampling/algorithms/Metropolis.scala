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
import scalismo.sampling.loggers.{ AcceptRejectLogger, SilentLogger }

import scala.math.exp
import scala.util.Random

/**
 * Metropolis algorithm (MCMC), provides samples from the evaluator distribution by drawing from generator and stochastic accept/reject decisions
 * generator needs to be symmetric
 */
class Metropolis[A] protected (val generator: ProposalGenerator[A] with SymmetricTransitionRatio[A],
    val evaluator: DistributionEvaluator[A],
    val logger: AcceptRejectLogger[A])(implicit val random: Random) extends MarkovChain[A] {
  // next sample
  override def next(current: A): A = {
    // reference p value
    val currentP = evaluator.logValue(current)
    // proposal
    val proposal = generator.propose(current)
    val proposalP = evaluator.logValue(proposal)
    // acceptance probability
    val a = proposalP - currentP
    // accept or reject
    if (a > 0.0 || random.nextDouble() < exp(a)) {
      logger.accept(current, proposal, generator, evaluator)
      proposal
    } else {
      logger.reject(current, proposal, generator, evaluator)
      current
    }
  }
}

object Metropolis {
  /** create a Metropolis MCMC chain, needs a symmetric proposal distribution, with logger attached */
  def apply[A](generator: ProposalGenerator[A] with SymmetricTransitionRatio[A],
    evaluator: DistributionEvaluator[A],
    logger: AcceptRejectLogger[A])(implicit random: Random) = new Metropolis[A](generator, evaluator, logger)

  /** create a Metropolis MCMC chain without a logger */
  def apply[A](generator: ProposalGenerator[A] with SymmetricTransitionRatio[A],
    evaluator: DistributionEvaluator[A])(implicit random: Random) = new Metropolis[A](generator, evaluator, new SilentLogger[A]())
}

/** Metropolis-Hastings algorithm - generates random samples from a target distribution using only samples from a proposal distribution */
class MetropolisHastings[A] protected (val generator: ProposalGenerator[A] with TransitionRatio[A],
    val evaluator: DistributionEvaluator[A],
    val logger: AcceptRejectLogger[A])(implicit val random: Random) extends MarkovChain[A] {
  /** next sample in Markov Chain */
  override def next(current: A): A = {
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
    if (a > 0.0 || random.nextDouble() < exp(a)) {
      logger.accept(current, proposal, generator, evaluator)
      proposal
    } else {
      logger.reject(current, proposal, generator, evaluator)
      current
    }
  }
}

object MetropolisHastings {
  def apply[A](generator: ProposalGenerator[A] with TransitionRatio[A],
    evaluator: DistributionEvaluator[A],
    logger: AcceptRejectLogger[A])(implicit random: Random) = new MetropolisHastings[A](generator, evaluator, logger)

  def apply[A](generator: ProposalGenerator[A] with TransitionRatio[A],
    evaluator: DistributionEvaluator[A])(implicit random: Random) = new MetropolisHastings[A](generator, evaluator, new SilentLogger[A]())
}

