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

/** 1D Gaussian distribution */
case class GaussianEvaluator(mean: Double, sdev: Double) extends DistributionEvaluator[Double] {
  val normalizer = -0.5 * math.log(2 * math.Pi) - math.log(sdev)

  /** log probability/density of sample */
  override def logValue(sample: Double): Double = -0.5 * (sample - mean) * (sample - mean) / sdev / sdev + normalizer
}

object GaussianEvaluator {
  /** probability density of 1D Gaussian distribution */
  def probability(x: Double, mean: Double, sdev: Double): Double = {
    -0.5 * math.log(2 * math.Pi) - math.log(sdev) - 0.5 * (x - mean) * (x - mean) / sdev / sdev
  }
}
