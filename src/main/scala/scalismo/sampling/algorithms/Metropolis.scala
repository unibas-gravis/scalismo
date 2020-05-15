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

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.math.exp

import scala.language.postfixOps

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
    implicit
    random: Random
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
    implicit
    random: Random
  ) = new MetropolisHastings[A](generator, evaluator)
}

/** Metropolis-Hastings algorithm with speculative executing -
 * generates random samples from a target distribution using only samples
 * from a proposal distribution
 *
 * The prefetching strategy precomputes (part of) the tree of possible future states and
 * evaluates the states in parallel. If proposals are cheap to compute but evaluation
 * expensive (and single threaded), this strategy might lead to a speed improvement,
 * especially when the number of acceptance is quite low.
 *
 * See for example
 * Accelerating Metropolis–Hastings algorithms: Delayed acceptance with prefetching
 * M. Banterle, C. Grazian, C .Robert
 * https://arxiv.org/pdf/1406.2660.pdf
 * for a good discussion of the idea and possibilities for further improvements.
 *
 * @param generator The proposal generator
 * @param evaluator The evaluator used to compute the (log) probability
 * @param numberOfParallelEvaluations The number of
 * */
class MetropolisHastingsWithPrefetching[A] protected (
  val generator: ProposalGenerator[A] with TransitionRatio[A],
  val evaluator: DistributionEvaluator[A],
  val numberOfParallelEvaluations: Int
)(implicit val random: Random)
    extends MarkovChain[A] {

  implicit val executionContext = ExecutionContext.global

  private lazy val silentLogger = new SilentLogger[A]()
  private val prefetchQueue = mutable.Queue[A]()

  /** start a logged iterator */
  def iterator(start: A, logger: AcceptRejectLogger[A]): Iterator[A] = Iterator.iterate(start) { next(_, logger) }

  // next sample
  def next(current: A, logger: AcceptRejectLogger[A]): A = {
    if (prefetchQueue.isEmpty) {
      fillPrefetchQueue(current, logger)
    }
    prefetchQueue.dequeue()
  }

  /*
   * Fills the prefetch queue with new proposals.
   * The method guarantees that after the call the prefetch queue
   * contains at least one element.
   */
  private def fillPrefetchQueue(current: A, logger: AcceptRejectLogger[A]): Unit = {
    val currentP = evaluator.logValue(current)

    case class EvaluatedProposal(proposal: A, logprob: Double, transitionRatio: Double)

    val proposals = for (_ <- (0 until numberOfParallelEvaluations)) yield {
      generator.propose(current)
    }

    // Evaluation can happen in parallel - no state is changed here
    val evaluatedProposalsFutures = for (proposal <- proposals) yield {
      Future(
        EvaluatedProposal(proposal, evaluator.logValue(proposal), generator.logTransitionRatio(current, proposal))
      )
    }

    // we wait for all evaluators to complete. As the same evaluator is run on all
    // they all need approximately the same time. We cap it at one minute, as no reasonable
    // MH-algorithm will ever have proposals that need to compute for more than 1 minute
    val evaluatedProposals = Await.result(Future.sequence(evaluatedProposalsFutures), 1 minute)

    // Now we run the normal metropolis acceptance test. If rejected, we
    // can go on and also use the next prefetched sample. If it is accepted,
    // we have to throw away all other samples
    var firstAccepted = false
    val it = evaluatedProposals.toSeq.iterator
    while (it.hasNext && !firstAccepted) {
      val evaluatedProposal = it.next()

      val a = evaluatedProposal.logprob - currentP - evaluatedProposal.transitionRatio

      if (a > 0.0 || random.scalaRandom.nextDouble() < exp(a)) {
        logger.accept(current, evaluatedProposal.proposal, generator, evaluator)
        prefetchQueue.enqueue(evaluatedProposal.proposal)
        firstAccepted = true
      } else {
        logger.reject(current, evaluatedProposal.proposal, generator, evaluator)
        prefetchQueue.enqueue(current)
      }
    }

  }

  /** next sample in chain */
  override def next(current: A): A = next(current, silentLogger)
}

object MetropolisHastingsWithPrefetching {
  def apply[A](generator: ProposalGenerator[A] with TransitionRatio[A],
               evaluator: DistributionEvaluator[A],
               numberOfParallelEvaluations: Int = Runtime.getRuntime.availableProcessors())(
    implicit random: Random
  ) = new MetropolisHastingsWithPrefetching[A](generator, evaluator, numberOfParallelEvaluations)
}
