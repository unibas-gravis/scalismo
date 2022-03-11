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
package scalismo.sampling

import scalismo.utils.Memoize

/** a probability or density */
trait DistributionEvaluator[A] {

  /** log probability/density of sample */
  def logValue(sample: A): Double
}

/** gradient evaluator, used together with DistributionEvaluator */
trait GradientEvaluator[A] {

  /** gradient at sample */
  def gradient(sample: A): A
}

trait MHDistributionEvaluator[A] extends DistributionEvaluator[MHSample[A]] {

  /** log probability/density of sample */
  def logValue(sample: MHSample[A]): Double

  def cached: MHDistributionEvaluator[A] = {
    new CachedMHEvaluator[A](this)
  }
}

case class CachedMHEvaluator[A](evaluator: MHDistributionEvaluator[A]) extends MHDistributionEvaluator[A] {
  val memoizedLogValue = Memoize(evaluator.logValue, 10)

  override def logValue(sample: MHSample[A]): Double = {
    memoizedLogValue(sample)
  }
}
