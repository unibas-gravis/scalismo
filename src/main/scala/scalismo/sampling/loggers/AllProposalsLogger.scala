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
package scalismo.sampling.loggers

import scalismo.sampling.{ DistributionEvaluator, ProposalGenerator }
import scala.language.implicitConversions

class AllProposalsLogger[A](logger: ChainStateLogger[A]) extends AcceptRejectLogger[A] {
  override def accept(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = logger.logState(sample)

  override def reject(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = logger.logState(sample)
}

class AcceptedProposalLogger[A](logger: ChainStateLogger[A]) extends AcceptRejectLogger[A] {
  private var state: Option[A] = None // keep track of last accepted proposal -> this is the state

  override def accept(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = {
    state = Some(sample)
    logger.logState(sample)
  }

  override def reject(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit = {
    if (state.isDefined) logger.logState(state.get)
  }
}

object AllProposalsLogger {
  // implicit attachment to a chain logger
  implicit def stateLoggerOnAllProposals[A](logger: ChainStateLogger[A]): RichChainStateLogger[A] = new RichChainStateLogger[A](logger)

  class RichChainStateLogger[A](logger: ChainStateLogger[A]) {
    def onAllProposals: AcceptRejectLogger[A] = new AllProposalsLogger[A](logger)
    def onAccepted: AcceptRejectLogger[A] = new AcceptedProposalLogger[A](logger)
  }
}
