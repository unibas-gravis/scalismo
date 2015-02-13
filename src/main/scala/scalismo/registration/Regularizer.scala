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
package scalismo.registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector

trait Regularizer extends (ParameterVector => Double) {

  def takeDerivative(p: ParameterVector): DenseVector[Float]
}

object RKHSNormRegularizer extends Regularizer {
  def apply(alpha: ParameterVector) = { val t = breeze.linalg.norm(alpha, 2); t * t }

  def takeDerivative(alpha: ParameterVector) = alpha * 2f

}