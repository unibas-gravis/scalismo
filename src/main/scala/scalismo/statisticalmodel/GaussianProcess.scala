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
import scalismo.common.DiscreteDomain.CanBound
import scalismo.common._
import scalismo.geometry._
import scalismo.kernels._
import scalismo.utils.Memoize

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
class GaussianProcess[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace] protected (val mean: VectorField[D, DO],
    val cov: MatrixValuedPDKernel[D, DO]) {

  protected[this] val dimOps: NDSpace[DO] = implicitly[NDSpace[DO]]

  private[this] def outputDimensionality = dimOps.dimensionality

  def domain = Domain.intersection(mean.domain, cov.domain)

  /**
   *
   * Sample values of the GAussian process evaluated at the given points.
   */
  def sampleAtPoints(pts: IndexedSeq[Point[D]]): DiscreteVectorField[D, DO] = {

    // define the mean and kernel matrix for the given points and construct the
    // corresponding MV Normal distribution, from which we then sample

    val mu = DenseVector.zeros[Float](pts.size * outputDimensionality)
    for ((pt, i) <- pts.zipWithIndex) {
      mu(i * outputDimensionality until (i + 1) * outputDimensionality) := mean(pt).toBreezeVector
    }

    val K = Kernel.computeKernelMatrix(pts, cov)
    val mvNormal = MultivariateNormalDistribution(mu, K)

    val sampleVec = mvNormal.drawSample()

    // The sample is a vector. We convert it back to a discreteVectorField.
    val vecs = sampleVec.toArray.grouped(outputDimensionality)
      .map(data => Vector[DO](data.map(_.toFloat)))
      .toIndexedSeq
    val domain = DiscreteDomain.fromSeq(pts.toIndexedSeq)
    DiscreteVectorField(domain, vecs)
  }

  /**
   * Compute the marginal distribution for the given point
   */
  def marginal(pt: Point[D]): NDimensionalNormalDistribution[DO] = NDimensionalNormalDistribution(mean(pt), cov(pt, pt))

  /**
   * The posterior distribution of the gaussian process, with respect to the given trainingData.
   * It is computed using Gaussian process regression.
   * We assume that the trainingData is subject to isotropic Gaussian noise with variance sigma2.
   */
  def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO])], sigma2: Double): GaussianProcess[D, DO] = {
    val cov = NDimensionalNormalDistribution[DO](Vector.zeros[DO], SquareMatrix.eye[DO] * sigma2)
    val fullTrainingData = trainingData.map { case (p, v) => (p, v, cov) }
    GaussianProcess.regression(this, fullTrainingData)
  }

  /**
   * The posterior distribution of the gaussian process, with respect to the given trainingData.
   * It is computed using Gaussian process regression.
   */
  def posterior(trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): GaussianProcess[D, DO] = {
    GaussianProcess.regression(this, trainingData)
  }
}

/**
 * Factory methods for createing Gaussian processes
 */
object GaussianProcess {

  /**
   * Creates a new Gaussian process with given mean and covariance, which is defined on the given domain.
   */
  def apply[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace](mean: VectorField[D, DO], cov: MatrixValuedPDKernel[D, DO]) = {
    new GaussianProcess[D, DO](mean, cov)
  }

  /**
   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
   *
   * @param gp  The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   */
  def regression[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace](gp: GaussianProcess[D, DO],
    trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): GaussianProcess[D, DO] = {

    val outputDim = implicitly[NDSpace[DO]].dimensionality

    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.data).toArray)

    val (xs, ys, errorDists) = trainingData.unzip3

    val mVec = flatten(xs.map(gp.mean))
    val yVec = flatten(ys)
    val fVec = yVec - mVec

    val K = Kernel.computeKernelMatrix(xs, gp.cov)
    for ((errorDist, i) <- errorDists.zipWithIndex) {
      K(i * outputDim until (i + 1) * outputDim, i * outputDim until (i + 1) * outputDim) += errorDist.cov.toBreezeMatrix
    }

    val K_inv = breeze.linalg.inv(K)

    def xstar(x: Point[D]) = { Kernel.computeKernelVectorFor[D, DO](x, xs, gp.cov) }

    def posteriorMean(x: Point[D]): Vector[DO] = {
      Vector[DO](((xstar(x) * K_inv).map(_.toFloat) * fVec).toArray)
    }

    val posteriorKernel = new MatrixValuedPDKernel[D, DO] {
      override def domain = gp.domain
      override def k(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
        gp.cov(x, y) - SquareMatrix[DO]((xstar(x) * K_inv * xstar(y).t).data.map(_.toFloat))
      }
    }

    new GaussianProcess[D, DO](VectorField(gp.domain, posteriorMean _), posteriorKernel)
  }

}
