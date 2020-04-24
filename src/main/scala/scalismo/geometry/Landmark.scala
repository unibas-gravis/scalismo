/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
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
package scalismo.geometry

import scalismo.transformations.Transformation
import scalismo.statisticalmodel.MultivariateNormalDistribution

case class Landmark[D: NDSpace](id: String,
                                point: Point[D],
                                description: Option[String] = None,
                                uncertainty: Option[MultivariateNormalDistribution] = None) {

  /**
   * Transforms a landmark point with the given transformations.
   * The method transforms both the point and the uncertainty. The new uncertainty is
   * estimated stochastically and is only an approximation to the real uncertainty
   * (for non-rigid transformations, the uncertainty would not even be gaussian)
   */
  def transform(transformation: Transformation[D])(implicit random: scalismo.utils.Random): Landmark[D] = {
    val transformedPoint = transformation(point)
    val transformedUncertainty = uncertainty match {
      case Some(uncertainty) => {

        // in order to transform the uncertainty, we simulate random landmark points
        // and estimate their distribution.
        val transformedPoints = for (i <- 0 until 5000) yield {
          val sampledPoint = point + NDSpace[D].createVector(uncertainty.sample()(random).toArray)
          transformation(sampledPoint)
        }
        val newUncertainty = MultivariateNormalDistribution.estimateFromData(transformedPoints.map(_.toBreezeVector))
        val newMean = newUncertainty.mean - transformedPoint.toBreezeVector
        Some(MultivariateNormalDistribution(newMean, newUncertainty.cov))
      }
      case None => None
    }
    this.copy(point = transformedPoint, uncertainty = transformedUncertainty)
  }
}
