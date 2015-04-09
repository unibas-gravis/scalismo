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

package scalismo.statisticalmodel

import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.common.DiscreteDomain.CanBound
import scalismo.common.{ VectorField, DiscreteDomain, DiscreteVectorField }
import scalismo.geometry.{ SquareMatrix, Vector, Dim, NDSpace }
import scalismo.kernels.{ MatrixValuedPDKernel, Kernel, DiscreteMatrixValuedPDKernel }

/**
 * A representation of a gaussian process, which is only defined on a discrete domain.
 * While this is technically similar to a MultivariateNormalDistribution, we highlight with this
 * class that we represent (discrete) functions, defined on the given domain.
 */
class DiscreteGaussianProcess[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace] private[scalismo] (val mean: DiscreteVectorField[D, DO],
    val cov: DiscreteMatrixValuedPDKernel[D, DO]) {

  require(mean.domain == cov.domain)

  val domain = mean.domain

  val outputDimensionality = implicitly[NDSpace[D]].dimensionality

  def sample: DiscreteVectorField[D, DO] = {
    // define the mean and kernel matrix for the given points and construct the
    // corresponding MV Normal distribution, from which we then sample

    val d = outputDimensionality

    val mu = mean.asBreezeVector
    val K = cov.asBreezeMatrix

    val mvNormal = MultivariateNormalDistribution(mu, K)

    val sampleVec = mvNormal.drawSample()

    // The sample is a vector. We convert it back to a discreteVectorField.
    val vecs = sampleVec.toArray.grouped(outputDimensionality)
      .map(data => Vector[DO](data.map(_.toFloat)))
      .toIndexedSeq
    DiscreteVectorField(domain, vecs)
  }

  /**
   * The marginal distribution at a given (single) point, specified by the pointId.
   */
  def marginal(pointId: Int) = {
    NDimensionalNormalDistribution(mean(pointId), cov(pointId, pointId))
  }

  /**
   * The marginal distribution for the points specified by the given point ids.
   * Note that this is again a DiscreteGaussianProcess.
   */
  def marginal(pointIds: Seq[Int]): DiscreteGaussianProcess[D, DO] = {
    val domainPts = domain.points.toIndexedSeq

    val newPts = pointIds.map(id => domainPts(id)).toIndexedSeq
    val newDomain = DiscreteDomain.fromSeq(newPts)

    val newMean = DiscreteVectorField(newDomain, pointIds.toIndexedSeq.map(id => mean(id)))
    val newCov = (i: Int, j: Int) => {
      cov(pointIds(i), pointIds(j))
    }
    val newDiscreteCov = DiscreteMatrixValuedPDKernel(newDomain, newCov)

    new DiscreteGaussianProcess(newMean, newDiscreteCov)
  }

}

object DiscreteGaussianProcess {

  def apply[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace](mean: DiscreteVectorField[D, DO], cov: DiscreteMatrixValuedPDKernel[D, DO]) = {
    new DiscreteGaussianProcess[D, DO](mean, cov)
  }
}