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

import scalismo.sampling.{DistributionEvaluator, MHDistributionEvaluator, MHSample}

/**
 * evaluate a product of distributions
 *
 * @param evaluators
 *   Sequence of distributions to evaluate
 */
class MHProductEvaluator[A](evaluators: Seq[MHDistributionEvaluator[A]]) extends MHDistributionEvaluator[A] {
  override def logValue(sample: MHSample[A]): Double = {
    evaluators.iterator.map(e => e.logValue(sample)).sum
  }
}

object MHProductEvaluator {
  def apply[A](evaluators: MHDistributionEvaluator[A]*) = new MHProductEvaluator[A](evaluators.toSeq)
}
