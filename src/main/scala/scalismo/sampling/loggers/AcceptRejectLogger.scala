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

trait AcceptRejectLogger[A] {
  def accept(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit

  def reject(current: A, sample: A, generator: ProposalGenerator[A], evaluator: DistributionEvaluator[A]): Unit
}

object AcceptRejectLogger {

  // Rich logger to attach various others

  object implicits {
    implicit def richLogger[A](logger: AcceptRejectLogger[A]): RichLogger[A] = new RichLogger[A](logger)

    class RichLogger[A](logger: AcceptRejectLogger[A]) {
      def subSampled(stepping: Int) = SteppedAcceptRejectLogger(stepping, logger)
    }

  }

}
