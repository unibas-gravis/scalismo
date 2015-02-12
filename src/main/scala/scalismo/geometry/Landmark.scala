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

import scalismo.io.LandmarkIO
import scalismo.statisticalmodel.NDimensionalNormalDistribution

/* It's explicitly forbidden to extend this class (which is possible, but ugly anyway),
 * to force "extensions" to use composition instead of subclassing.
 *
 * The primary reason is to force implementations to provide the correct extension encode/decode functions when using
 * the LandmarkIO methods for reading and writing JSON representations of landmarks.
 */
final case class Landmark[D <: Dim: NDSpace](id: String, point: Point[D], description: Option[String] = None, uncertainty: Option[NDimensionalNormalDistribution[D]] = None)

object Landmark {

  import scala.language.implicitConversions

  implicit def noExtensionsEncodeFunction[D <: Dim: NDSpace]: LandmarkIO.ExtensionEncodeFunction[D, Landmark[D]] = { lm => (lm, None) }

  implicit def noExtensionsDecodeFunction[D <: Dim: NDSpace]: LandmarkIO.ExtensionDecodeFunction[D, Landmark[D]] = {
    case (lm, _) => lm
  }
}