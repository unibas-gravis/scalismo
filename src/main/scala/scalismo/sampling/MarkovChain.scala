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

import scalismo.sampling.loggers.ChainStateLogger

/** basic Markov Chain trait: provides a next sample */
trait MarkovChain[A] {
  /** next sample in chain */
  def next(current: A): A

  /** provide a chain starting from current as an iterator */
  def iterator(current: A): Iterator[A] = Iterator.iterate(current)(next)
}

object MarkovChain {
  /** fat interface with rich enhancements for Markov Chains - only implementation, not a type */
  implicit class RichMarkovChain[A](mc: MarkovChain[A]) {
    def loggedWith(logger: ChainStateLogger[A]): MarkovChain[A] = new LoggedMarkovChain[A](mc, logger)
  }

  class LoggedMarkovChain[A](chain: MarkovChain[A], logger: ChainStateLogger[A]) extends MarkovChain[A] {
    /** next sample in chain */
    override def next(current: A): A = {
      val sample = chain.next(current)
      logger.logState(sample)
      sample
    }

    override def toString = chain.toString
  }
}