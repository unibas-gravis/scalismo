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

import breeze.linalg.DenseVector
import scalismo.geometry.Dim

/**
 * This trait defines the interface for regularizers in the scalismo registration framework.
 * A regularizer is defined as a differentiable functions of the registration parameters.
 *
 */
trait Regularizer[D <: Dim] {

  def transformationSpace: TransformationSpace[D]

  def value(p: DenseVector[Double]): Double

  def takeDerivative(p: DenseVector[Double]): DenseVector[Double]
}

/**
 * A regularizer which simply penalizes the squared norm of the parameters.
 */
case class L2Regularizer[D <: Dim](transformationSpace: TransformationSpace[D]) extends Regularizer[D] {

  def value(alpha: DenseVector[Double]) = { val t = breeze.linalg.norm(alpha, 2); t * t }

  def takeDerivative(alpha: DenseVector[Double]) = alpha * 2.0

}