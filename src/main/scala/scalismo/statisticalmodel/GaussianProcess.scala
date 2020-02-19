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
import scalismo.geometry._
import scalismo.kernels._
import scalismo.utils.Random

/**
 * A gaussian process from a D dimensional input space, whose input values are points,
 * to a DO dimensional output space. The output space is a Euclidean vector space of dimensionality DO.
 *
 * @param mean The mean function
 * @param cov  The covariance function. Needs to be positive definite
 * @tparam D The dimensionality of the input space
 */
class GaussianProcess[D: NDSpace, Value] protected (val mean: Field[D, Value], val cov: MatrixValuedPDKernel[D])(
  implicit val vectorizer: Vectorizer[Value]
) {

  def outputDim = vectorizer.dim

  def domain = Domain.intersection(mean.domain, cov.domain)

  /**
   *
   * Sample values of the Gaussian process evaluated at the given points.
   */
  def sampleAtPoints[DDomain <: DiscreteDomain[D]](
    domain: DDomain
  )(implicit rand: Random): DiscreteField[D, DDomain, Value] = {
    this.marginal(domain).sample()
  }

  /**
   * Compute the marginal distribution for the given points. The result is again a Gaussian process, whose domain
   * is defined by the given points.
   */
  def marginal[DDomain <: DiscreteDomain[D]](domain: DDomain): DiscreteGaussianProcess[D, DDomain, Value] = {
    val meanField = DiscreteField[D, DDomain, Value](domain, domain.points.toIndexedSeq.map(pt => mean(pt)))
    val pts = domain.points.toIndexedSeq
    def newCov(i: PointId, j: PointId): DenseMatrix[Double] = {
      cov(pts(i.id), pts(j.id))
    }

    val discreteCov = DiscreteMatrixValuedPDKernel[D](domain, newCov, outputDim)
    new DiscreteGaussianProcess(meanField, discreteCov)
  }

  /**
   * Compute the marginal distribution at a single point.
   */
  def marginal(pt: Point[D]) = MultivariateNormalDistribution(vectorizer.vectorize(mean(pt)), cov(pt, pt))

  /**
   * The posterior distribution of the gaussian process, with respect to the given trainingData.
   * It is computed using Gaussian process regression.
   * We assume that the trainingData is subject to isotropic Gaussian noise with variance sigma2.
   */
  def posterior(trainingData: IndexedSeq[(Point[D], Value)], sigma2: Double): GaussianProcess[D, Value] = {
    val cov =
      MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) * sigma2)
    val fullTrainingData = trainingData.map { case (p, v) => (p, v, cov) }
    GaussianProcess.regression(this, fullTrainingData)
  }

  /**
   * The posterior distribution of the gaussian process, with respect to the given trainingData.
   * It is computed using Gaussian process regression.
   */
  def posterior(
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
  ): GaussianProcess[D, Value] = {
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
  def apply[D: NDSpace, Value](mean: Field[D, Value], cov: MatrixValuedPDKernel[D])(
    implicit vectorizer: Vectorizer[Value]
  ): GaussianProcess[D, Value] = {
    new GaussianProcess[D, Value](mean, cov)
  }

  /**
   * Creates a new zero-mean Gaussian process with the given covariance function.
   */
  def apply[D: NDSpace, Value](
    cov: MatrixValuedPDKernel[D]
  )(implicit vectorizer: Vectorizer[Value]): GaussianProcess[D, Value] = {
    val zeroVec = vectorizer.unvectorize(DenseVector.zeros(vectorizer.dim))
    val zeroField = Field[D, Value](RealSpace[D], (p: Point[D]) => zeroVec)
    GaussianProcess[D, Value](zeroField, cov)
  }

  /**
   * * Performs a Gaussian process regression, where we assume that each training point (vector) is subject to  zero-mean noise with given variance.
   *
   * @param gp           The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   */
  def regression[D: NDSpace, Value](
    gp: GaussianProcess[D, Value],
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
  )(implicit vectorizer: Vectorizer[Value]): GaussianProcess[D, Value] = {

    val outputDim = vectorizer.dim

    val (xs, ys, errorDists) = trainingData.unzip3

    val mVec = DiscreteField.vectorize[D, Value](xs.map(gp.mean))
    val yVec = DiscreteField.vectorize[D, Value](ys)
    val fVec = yVec - mVec

    val K = Kernel.computeKernelMatrix(xs, gp.cov)
    for ((errorDist, i) <- errorDists.zipWithIndex) {
      K(i * outputDim until (i + 1) * outputDim, i * outputDim until (i + 1) * outputDim) += errorDist.cov
    }

    val K_inv = breeze.linalg.inv(K)

    def xstar(x: Point[D]) = {
      Kernel.computeKernelVectorFor[D](x, xs, gp.cov)
    }

    def posteriorMean(x: Point[D]): Value = {
      vectorizer.unvectorize((xstar(x) * K_inv) * fVec)
    }

    val posteriorKernel = new MatrixValuedPDKernel[D] {
      override def domain = gp.domain

      override def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
        gp.cov(x, y) - (xstar(x) * K_inv * xstar(y).t)
      }

      override def outputDim = gp.outputDim
    }

    new GaussianProcess[D, Value](Field(gp.domain, posteriorMean _), posteriorKernel)
  }

  /**
   * * Computes the marginal likelihood of the observed data, according to the given GP.
   *
   * This can for example be used in a model selection setting, where the GP with the maximum marginal likelihood of the observed data would be selected.
   *
   * @param gp           The gaussian process
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   * @todo The current implementation can be optimized as it inverts the data covariance matrix (that can be heavy for more than a few points). Instead an implementation
   *       with a Cholesky decomposition would be more efficient.
   */
  def marginalLikelihood[D: NDSpace, Value](
    gp: GaussianProcess[D, Value],
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]
  )(implicit vectorizer: Vectorizer[Value]): Double = {

    val outputDim = gp.outputDim

    // below is the implementation according to Rassmussen Gaussian Processes, Chapter 5, page 113

    val (xs, ys, errorDistributions) = trainingData.unzip3
    val meanValues = xs.map(gp.mean)
    val mVec = DiscreteField.vectorize[D, Value](meanValues)
    val yVec = DiscreteField.vectorize[D, Value](ys)
    val yVecZeroMean = yVec - mVec

    val Ky = DenseMatrix.zeros[Double](trainingData.size * outputDim, trainingData.size * outputDim)

    for ((xI, i) <- xs.zipWithIndex; (xJ, j) <- xs.zipWithIndex) {

      val covBlock = gp.cov(xI, xJ)

      val Kyyp = if (i == j) {
        // in this case add observation uncertainty
        val noiseBlock = errorDistributions(i).cov
        covBlock + noiseBlock
      } else covBlock

      // insert the block in the big covariance matrix
      for (l <- 0 until outputDim; m <- 0 until outputDim) {
        Ky(l + (i * outputDim), m + (j * outputDim)) = Kyyp(l, m)
      }
    }

    val KyInv = inv(Ky)
    val const = trainingData.length * 0.5 * math.log(math.Pi * 2)
    val margLikehood = ((yVecZeroMean.t * KyInv * yVecZeroMean) * -0.5) - (0.5 * math.log(det(Ky))) - const
    margLikehood
  }

}
