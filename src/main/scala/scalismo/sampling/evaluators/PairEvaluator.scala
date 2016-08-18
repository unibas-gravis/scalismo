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
package scalismo.sampling.evaluators

import scalismo.sampling.DistributionEvaluator

/** probability density based on pairs, e.g. Gaussian centered on first evaluated at second */
trait PairEvaluator[A] {
  self =>
  /** probability/density value for a pair (log value) */
  def logValue(first: A, second: A): Double

  /** make a distribution evaluator, centered at first */
  def toDistributionEvaluator(first: A): DistributionEvaluator[A] = new DistributionEvaluator[A] {
    override def logValue(sample: A): Double = self.logValue(first, sample)

    override def toString = self.toString
  }

  /** make a distribution evaluator for tuples to encode pairs */
  def tupled = new DistributionEvaluator[(A, A)] {
    override def logValue(sample: (A, A)): Double = self.logValue(sample._1, sample._2)

    override def toString = self.toString
  }
}
