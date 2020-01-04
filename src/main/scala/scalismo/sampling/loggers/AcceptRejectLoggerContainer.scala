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

import scala.language.implicitConversions

/** container for multiple AcceptRejectLoggers */
class AcceptRejectLoggerContainer[A](loggers: Seq[AcceptRejectLogger[A]]) extends AcceptRejectLogger[A] {
  override def accept(current: A,
                      sample: A,
                      generator: ProposalGenerator[A],
                      evaluator: DistributionEvaluator[A]): Unit = {
    loggers.foreach(_.accept(current, sample, generator, evaluator))
  }

  override def reject(current: A,
                      sample: A,
                      generator: ProposalGenerator[A],
                      evaluator: DistributionEvaluator[A]): Unit = {
    loggers.foreach(_.reject(current, sample, generator, evaluator))
  }
}

object AcceptRejectLoggerContainer {
  def apply[A](loggers: Seq[AcceptRejectLogger[A]]) = new AcceptRejectLoggerContainer[A](loggers)

  /** implicit building DSL: logger1 :+ logger2 -> LoggerContainer */
  object implicits {
    implicit def promoteLoggerToContainerBuilder[A](logger: AcceptRejectLogger[A]): ContainerBuilder[A] =
      new ContainerBuilder[A](Seq(logger))

    implicit def buildContainer[A](containerBuilder: ContainerBuilder[A]): AcceptRejectLoggerContainer[A] =
      containerBuilder.toContainer

    class ContainerBuilder[A](logger: Seq[AcceptRejectLogger[A]]) {
      def :+(other: AcceptRejectLogger[A]): ContainerBuilder[A] = new ContainerBuilder[A](logger :+ other)

      def toContainer: AcceptRejectLoggerContainer[A] = new AcceptRejectLoggerContainer[A](logger)
    }

  }

}

/** container to keep multiple ChainStateLoggers active */
class ChainStateLoggerContainer[A](loggers: Seq[ChainStateLogger[A]]) extends ChainStateLogger[A] {
  override def logState(sample: A): Unit = {
    loggers.foreach(_.logState(sample))
  }
}

object ChainStateLoggerContainer {
  def apply[A](loggers: Seq[ChainStateLogger[A]]) = new ChainStateLoggerContainer[A](loggers)

  /** implicit construction with "logger :+ other" */
  object implicits {
    implicit def promoteToLoggerBuilder[A](logger: ChainStateLogger[A]): ContainerBuilder[A] =
      new ContainerBuilder[A](Seq(logger))

    implicit def buildContainer[A](builder: ContainerBuilder[A]): ChainStateLoggerContainer[A] = builder.toContainer

    class ContainerBuilder[A](loggers: Seq[ChainStateLogger[A]]) {
      def :+(other: ChainStateLogger[A]): ContainerBuilder[A] = new ContainerBuilder[A](loggers :+ other)

      def toContainer: ChainStateLoggerContainer[A] = new ChainStateLoggerContainer[A](loggers)
    }

  }

}
