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

import scalismo.geometry.{ Point, Dim }
import scalismo.statisticalmodel.LowRankGaussianProcess
import scalismo.statisticalmodel.LowRankGaussianProcess.Eigenpair

import scala.NotImplementedError
import TransformationSpace.ParameterVector

import breeze.linalg.{ DenseMatrix, DenseVector }

class GaussianProcessTransformationSpace[D <: Dim] private (gp: LowRankGaussianProcess[D, D]) extends TransformationSpace[D] with DifferentiableTransforms[D] {

  override type T = GaussianProcessTransformation[D]

  override def identityTransformParameters = DenseVector.zeros[Float](parametersDimensionality)

  override def parametersDimensionality = gp.rank

  override def transformForParameters(p: ParameterVector) = GaussianProcessTransformation[D](gp, p)
  override def takeDerivativeWRTParameters(p: ParameterVector) = {

    /**
     * The jacobian matrix of a sample, with respect to the given parameteers.
     * @param p
     * @return
     */
    (x: Point[D]) =>
      {
        val dim = x.dimensionality
        val J = DenseMatrix.zeros[Float](dim, gp.klBasis.size)
        (0 until gp.rank).map(i => {
          val Eigenpair(lambda_i, phi_i) = gp.klBasis(i)
          J(::, i) := (phi_i(x) * math.sqrt(lambda_i).toFloat).toBreezeVector
        })
        J
      }
  }

}

class GaussianProcessTransformation[D <: Dim] private (gp: LowRankGaussianProcess[D, D], alpha: ParameterVector) extends ParametricTransformation[D] with CanDifferentiate[D] {

  val instance = gp.instance(alpha)
  val parameters = alpha

  override val domain = gp.domain

  override val f = (x: Point[D]) => {
    val newPointAsVector = instance(x)
    x + newPointAsVector
  }
  def takeDerivative(x: Point[D]) = { throw new NotImplementedError("take derivative of kernel") }
}

object GaussianProcessTransformation {
  def apply[D <: Dim](gp: LowRankGaussianProcess[D, D], alpha: TransformationSpace.ParameterVector) = new GaussianProcessTransformation[D](gp, alpha)
}

object GaussianProcessTransformationSpace {
  def apply[D <: Dim](gp: LowRankGaussianProcess[D, D]) = new GaussianProcessTransformationSpace[D](gp)
}

