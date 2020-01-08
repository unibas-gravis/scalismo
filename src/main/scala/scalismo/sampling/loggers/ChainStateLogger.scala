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

import scala.language.implicitConversions

/* side-effects-based logging facility */

trait ChainStateLogger[A] extends RichLogger[A] {
  def logState(sample: A): Unit
}

trait RichLogger[A] {
  self: ChainStateLogger[A] =>
  def subSampled(stepping: Int): ChainStateLogger[A] = SteppedChainStateLogger(stepping, self)
}

object ChainStateLogger {

  /** implicit conversions to attach a logger to a chain iterator or sub sample a logger */
  object implicits {
    implicit def richLogger[A](logger: ChainStateLogger[A]): RichLogger[A] = new RichLogger[A](logger)

    class RichLogger[A](logger: ChainStateLogger[A]) {
      def subSampled(stepping: Int) = SteppedChainStateLogger(stepping, logger)
    }

    /** implicit attachment of a ChainStateLogger to an iterator */
    implicit class RichIterator[A](chainIterator: Iterator[A]) {
      def loggedWith(logger: ChainStateLogger[A]): Iterator[A] = {
        new Iterator[A] {
          override def hasNext: Boolean = chainIterator.hasNext
          override def next(): A = {
            val sample = chainIterator.next()
            logger.logState(sample)
            sample
          }
        }
      }
    }

  }
}
