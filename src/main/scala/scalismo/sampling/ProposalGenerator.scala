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

/** proposal distribution sampler for Metropolis-Hastings MCMC */
trait ProposalGenerator[A] {
  /** draw a sample from this proposal distribution, may depend on current state */
  def propose(current: A): A
}

/** ratio of forward and backwards proposal probability/density */
trait TransitionRatio[A] {
  /** total rate of transition from to, corrected for backwards transition (log value) */
  def logTransitionRatio(from: A, to: A): Double
}

/** symmetric transition: equal forward and backward transition probability */
trait SymmetricTransitionRatio[A] extends TransitionRatio[A] {
  override def logTransitionRatio(from: A, to: A) = 0.0
}

/** expresses transition probability between two states */
trait TransitionProbability[A] extends TransitionRatio[A] {
  /** rate of transition from to (log value) */
  def logTransitionProbability(from: A, to: A): Double

  /** transition ratio forward probability / backward probability */
  override def logTransitionRatio(from: A, to: A): Double = {
    val fw = logTransitionProbability(from, to)
    val bw = logTransitionProbability(to, from)

    if (fw.isNaN || bw.isNaN)
      throw new Exception("NaN transition Probability!")

    if (!fw.isNegInfinity || !bw.isNegInfinity)
      fw - bw
    else
      0.0
  }
}
