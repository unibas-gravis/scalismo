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

import scalismo.sampling.DistributionEvaluator

/** keep the best sample so far, warning: changes state */
class BestSampleLogger[A](evaluator: DistributionEvaluator[A]) extends ChainStateLogger[A] {

  private case class EvaluatedSample(sample: A, value: Double)

  private var bestState: Option[EvaluatedSample] = None

  override def logState(sample: A): Unit = {
    val value = evaluator.logValue(sample)
    if (bestState.isEmpty || value > bestState.get.value) {
      bestState = Some(EvaluatedSample(sample, value))
    }
  }

  def currentBestSample() = bestState.map(_.sample)

  def currentBestValue() = bestState.map(_.value)
}

object BestSampleLogger {
  def apply[A](evaluator: DistributionEvaluator[A]) = new BestSampleLogger[A](evaluator)
}
