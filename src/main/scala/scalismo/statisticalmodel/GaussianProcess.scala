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

import breeze.linalg.svd.SVD
import breeze.linalg.{ *, DenseVector, DenseMatrix }
import scalismo.common.{ FiniteDiscreteDomain, DiscreteVectorField, Domain, VectorField }
import scalismo.geometry._
import scalismo.kernels.{ Kernel, MatrixValuedPDKernel }

/**
 * A gaussian process from a D dimensional input space, whose input values are points,
 * to a DO dimensional output space. The output space is a Euclidean vector space of dimensionality DO.
 *
 * @param domain defines the set of points on which the GP is defined
 * @param mean The mean function
 * @param cov  The covariance function. Needs to be positive definite
 * @tparam D The dimensionality of the input space
 * @tparam DO The dimensionality of the output space
 */
class GaussianProcess[D <: Dim: NDSpace, DO <: Dim: NDSpace] protected (val mean: VectorField[D, DO],
    val cov: MatrixValuedPDKernel[D, DO]) {

  protected[this] val dimOps: NDSpace[DO] = implicitly[NDSpace[DO]]

  private[this] def outputDimensionality = dimOps.dimensionality

  def domain = Domain.intersection(mean.domain, cov.domain)

  /**
   *
   * Sample values of the GAussian process evaluated at the given points.
   */
  def sampleAtPoints(pts: IndexedSeq[Point[D]]): DiscreteVectorField[D, DO] = {
    val K = Kernel.computeKernelMatrix(pts, cov).map(_.toDouble)

    // TODO check that all points are part of the domain

    // TODO using the svd is slightly inefficient, but with the current version of breeze, the cholesky decomposition does not seem to work
    val SVD(u, s, _) = breeze.linalg.svd(K)
    val L = u.copy
    for (i <- 0 until s.size) {
      L(::, i) := u(::, i) * Math.sqrt(s(i))
    }
    val r = breeze.stats.distributions.Gaussian(0, 1)
    val nGaussians = for (i <- 0 until pts.size * outputDimensionality) yield r.draw()
    val v = DenseVector(nGaussians.toArray)
    val sampleVec = L * v
    val vecs = sampleVec.toArray.grouped(outputDimensionality)
      .map(data => Vector[DO](data.map(_.toFloat)))
      .toIndexedSeq
    val domain = FiniteDiscreteDomain.fromSeq(pts.toIndexedSeq)
    DiscreteVectorField(domain, vecs)
  }

  /**
   * Compute the marginal distribution for the given point
   */
  def marginal(pt: Point[D]): NDimensionalNormalDistribution[DO] = NDimensionalNormalDistribution(mean(pt), cov(pt, pt))
}

/**
 * Factory methods for createing Gaussian processes
 */
object GaussianProcess {

  /**
   * Creates a new Gaussian process with given mean and covariance, which is defined on the given domain.
   */
  def apply[D <: Dim: NDSpace, DO <: Dim: NDSpace](mean: VectorField[D, DO], cov: MatrixValuedPDKernel[D, DO]) = {
    new GaussianProcess[D, DO](mean, cov)
  }

}
