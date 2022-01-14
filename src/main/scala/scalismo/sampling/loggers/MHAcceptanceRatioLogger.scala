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

import scalismo.sampling.{DistributionEvaluator, MHSample, ProposalGenerator}


class MHAcceptanceRatioLogger[A] extends AcceptRejectLogger[MHSample[A]] {
  private val numAccepted = collection.mutable.Map[String, Int]()
  private val numRejected = collection.mutable.Map[String, Int]()

  override def accept(current: MHSample[A],
                      sample: MHSample[A],
                      generator: ProposalGenerator[MHSample[A]],
                      evaluator: DistributionEvaluator[MHSample[A]]
                     ): Unit = {
    val numAcceptedSoFar = numAccepted.getOrElseUpdate(sample.generatedBy, 0)
    numAccepted.update(sample.generatedBy, numAcceptedSoFar + 1)
  }

  override def reject(current: MHSample[A],
                      sample: MHSample[A],
                      generator: ProposalGenerator[MHSample[A]],
                      evaluator: DistributionEvaluator[MHSample[A]]
                     ): Unit = {
    val numRejectedSoFar = numRejected.getOrElseUpdate(sample.generatedBy, 0)
    numRejected.update(sample.generatedBy, numRejectedSoFar + 1)
  }



  def acceptanceRatios() : Map[String, Double] = {
    val generatorNames = numRejected.keys.toSet.union(numAccepted.keys.toSet)
    val acceptanceRatios = for (generatorName <- generatorNames ) yield {
      val total = (numAccepted.getOrElse(generatorName, 0)
        + numRejected.getOrElse(generatorName, 0)).toDouble
      (generatorName, numAccepted.getOrElse(generatorName, 0) / total)
    }
    acceptanceRatios.toMap
  }
}
