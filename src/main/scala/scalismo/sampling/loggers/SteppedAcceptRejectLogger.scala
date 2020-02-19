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

import scalismo.sampling.{DistributionEvaluator, ProposalGenerator}

class SteppedAcceptRejectLogger[A](stepping: Int, logger: AcceptRejectLogger[A]) extends AcceptRejectLogger[A] {
  private var counter = 0

  override def accept(current: A,
                      sample: A,
                      generator: ProposalGenerator[A],
                      evaluator: DistributionEvaluator[A]): Unit = {
    if (counter % stepping == 0)
      logger.accept(current, sample, generator, evaluator)
    counter += 1
  }

  override def reject(current: A,
                      sample: A,
                      generator: ProposalGenerator[A],
                      evaluator: DistributionEvaluator[A]): Unit = {
    if (counter % stepping == 0)
      logger.reject(current, sample, generator, evaluator)
    counter += 1
  }
}

object SteppedAcceptRejectLogger {
  def apply[A](stepping: Int, logger: AcceptRejectLogger[A]) = new SteppedAcceptRejectLogger[A](stepping, logger)
}

class SteppedChainStateLogger[A](stepping: Int, logger: ChainStateLogger[A]) extends ChainStateLogger[A] {
  private var counter = 0

  override def logState(sample: A): Unit = {
    if (counter % stepping == 0)
      logger.logState(sample)
    counter += 1
  }
}

object SteppedChainStateLogger {
  def apply[A](stepping: Int, logger: ChainStateLogger[A]) = new SteppedChainStateLogger[A](stepping, logger)
}
