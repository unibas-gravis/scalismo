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

import breeze.linalg._
import scalismo.common._
import scalismo.geometry.Vector
import scalismo.geometry._
import scalismo.kernels._

/**
 * A gaussian process from a D dimensional input space, whose input values are points,
 * to a DO dimensional output space. The output space is a Euclidean vector space of dimensionality DO.
 *
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
   * Sample values of the Gaussian process evaluated at the given points.
   */
  def sampleAtPoints(domain: DiscreteDomain[D]): DiscreteVectorField[D, DO] = {
    this.marginal(domain).sample
  }

  /**
   * Compute the marginal distribution for the given points. The result is again a Gaussian process, whose domain
   * is defined by the given points.
   */
  def marginal(domain: DiscreteDomain[D]): DiscreteGaussianProcess[D, DO] = {
    val meanField = DiscreteVectorField(domain, domain.points.toIndexedSeq.map(pt => mean(pt)))
    val pts = domain.points.toIndexedSeq
    def newCov(i: PointId, j: PointId): SquareMatrix[DO] = {
      cov(pts(i.id), pts(j.id))
    }

    val discreteCov = DiscreteMatrixValuedPDKernel[D, DO](domain, newCov)
    new DiscreteGaussianProcess(meanField, discreteCov)
  }

  /**
   * Compute the marginal distribution at a single point.
   */
  def marginal(pt: Point[D]) = NDimensionalNormalDistribution(mean(pt), cov(pt, pt))

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
 * Factory methods for creating Gaussian processes
 */
object GaussianProcess {

  /**
   * Creates a new Gaussian process with given mean and covariance, which is defined on the given domain.
   */
  def apply[D <: Dim: NDSpace, DO <: Dim: NDSpace](mean: VectorField[D, DO], cov: MatrixValuedPDKernel[D, DO]) = {
    new GaussianProcess[D, DO](mean, cov)
  }

  /**
   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
   *
   * @param gp  The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   */
  def regression[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: GaussianProcess[D, DO],
    trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): GaussianProcess[D, DO] = {

    val outputDim = implicitly[NDSpace[DO]].dimensionality

    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.toArray).toArray)

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
      Vector[DO](((xstar(x) * K_inv) * fVec).toArray)
    }

    val posteriorKernel = new MatrixValuedPDKernel[D, DO] {
      override def domain = gp.domain
      override def k(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
        gp.cov(x, y) - SquareMatrix[DO]((xstar(x) * K_inv * xstar(y).t).data)
      }
    }

    new GaussianProcess[D, DO](VectorField(gp.domain, posteriorMean _), posteriorKernel)
  }

  /**
   * * Computes the marginal likelihood of the observed data, according to the given GP.
   *
   * This can for example be used in a model selection setting, where the GP with the maximum marginal likelihood of the observed data would be selected.
   *
   * @param gp  The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   * @todo The current implementation can be optimized as it inverts the data covariance matrix (that can be heavy for more than a few points). Instead an implementation
   *       with a Cholesky decomposition would be more efficient.
   */
  def marginalLikelihood[D <: Dim: NDSpace, DO <: Dim: NDSpace](gp: GaussianProcess[D, DO],
    trainingData: IndexedSeq[(Point[D], Vector[DO], NDimensionalNormalDistribution[DO])]): Double = {

    val outputDim = implicitly[NDSpace[DO]].dimensionality

    // below is the implementation according to Rassmussen Gaussian Processes, Chapter 5, page 113

    val (ptIds, ys, errorDistributions) = trainingData.unzip3
    def flatten(v: IndexedSeq[Vector[DO]]) = DenseVector(v.flatten(_.toArray).toArray)
    val yVec = flatten(ys)

    val Ky = DenseMatrix.zeros[Double](trainingData.size * outputDim, trainingData.size * outputDim)

    for ((ptIdI, i) <- ptIds.zipWithIndex; (ptIdJ, j) <- ptIds.zipWithIndex) {

      val covBlock = gp.cov(ptIdI, ptIdJ).toBreezeMatrix

      val Kyyp = if (i == j) {
        // in this case add observation uncertainty
        val noiseBlock = errorDistributions(i).cov.toBreezeMatrix
        covBlock + noiseBlock
      } else covBlock

      // insert the block in the big covariance matrix
      for (l <- (0 until outputDim); m <- (0 until outputDim)) {
        Ky(l + (i * outputDim), m + (j * outputDim)) = Kyyp(l, m)
      }
    }

    val KyInv = inv(Ky)
    val const = trainingData.length * 0.5 * math.log(math.Pi * 2)
    val margLikehood = ((yVec.t * KyInv * yVec) * -0.5f) - (0.5f * math.log(det(Ky))) - const
    margLikehood
  }

}
