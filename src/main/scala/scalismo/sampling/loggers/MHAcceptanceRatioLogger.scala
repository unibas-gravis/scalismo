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

import scalismo.sampling.loggers.MHSampleLogger.{Accepted, LoggedMHSamples, MHSampleWithDecision, Rejected}
import scalismo.sampling.{DistributionEvaluator, MHSample, ProposalGenerator}

import scala.collection.mutable.ListBuffer

/**
 * Generic logger to log accepted and rejected samples in a Metropolis Hastings chain.
 */
class MHSampleLogger[A] extends AcceptRejectLogger[MHSample[A]] {

  private val sampleBuf: ListBuffer[MHSampleWithDecision[A]] = new ListBuffer[MHSampleWithDecision[A]]()

  override def accept(current: MHSample[A],
                      sample: MHSample[A],
                      generator: ProposalGenerator[MHSample[A]],
                      evaluator: DistributionEvaluator[MHSample[A]]
  ): Unit = {
    sampleBuf.append(MHSampleWithDecision(sample, Accepted))
  }

  override def reject(current: MHSample[A],
                      sample: MHSample[A],
                      generator: ProposalGenerator[MHSample[A]],
                      evaluator: DistributionEvaluator[MHSample[A]]
  ): Unit = {
    sampleBuf.append(MHSampleWithDecision(sample, Rejected))
  }

  def samples: LoggedMHSamples[A] = new LoggedMHSamples(sampleBuf.toSeq)
}

object MHSampleLogger {
  trait AcceptanceState
  case object Rejected extends AcceptanceState
  case object Accepted extends AcceptanceState

  case class MHSampleWithDecision[A](sample: MHSample[A], acceptanceState: AcceptanceState)

  def apply[A](): MHSampleLogger[A] = new MHSampleLogger[A]()

  class LoggedMHSamples[A](samples: Seq[MHSampleWithDecision[A]]) {

    def takeLast(n: Int): LoggedMHSamples[A] = new LoggedMHSamples[A](samples.takeRight(n))
    def dropFirst(n: Int): LoggedMHSamples[A] = new LoggedMHSamples[A](samples.drop(n))
    def accepted: Seq[MHSample[A]] = samples.collect { case MHSampleWithDecision(sample, Accepted) => sample }
    def rejected: Seq[MHSample[A]] = samples.collect { case MHSampleWithDecision(sample, Rejected) => sample }

    def acceptanceRatios: Map[String, Double] = {

      val generatorNames = samples.map(_.sample.generatedBy).toSet

      val acceptanceRatios = for (generatorName <- generatorNames) yield {
        val numAccepted = samples.count(s => s.sample.generatedBy == generatorName && s.acceptanceState == Accepted)
        val numRejected = samples.count(s => s.sample.generatedBy == generatorName && s.acceptanceState == Rejected)
        generatorName -> numAccepted / (numAccepted + numRejected).toDouble
      }

      acceptanceRatios.toMap
    }
  }

}
