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
class GaussianProcess[D: NDSpace, Value](val mean: Field[D, Value], val cov: MatrixValuedPDKernel[D])(
  implicit
  val vectorizer: Vectorizer[Value]
) {

  def outputDim = vectorizer.dim

  def domain = Domain.intersection(mean.domain, cov.domain)

  /**
   *
   * Sample values of the Gaussian process evaluated at the given points.
   */
  def sampleAtPoints[DDomain[DD] <: DiscreteDomain[DD]](
    domain: DDomain[D]
  )(implicit rand: Random): DiscreteField[D, DDomain, Value] = {
    this.discretize(domain).sample()
  }

  /**
   * Compute the marginal distribution for the given points. The result is again a Gaussian process, whose domain
   * is an unstructured points domain
   */
  def marginal(points: IndexedSeq[Point[D]])(
    implicit domainCreator: UnstructuredPointsDomain.Create[D]
  ): DiscreteGaussianProcess[D, UnstructuredPointsDomain, Value] = {
    val domain = domainCreator.create(points)
    discretize(domain)
  }

  /**
   * Compute the marginal distribution at a single point.
   */
  def marginal(pt: Point[D]) = MultivariateNormalDistribution(vectorizer.vectorize(mean(pt)), cov(pt, pt))

  /**
   * Computes the marginal likelihood of the observed data.
   *
   * This can for example be used in a model selection setting, where the GP with the maximum marginal likelihood of the observed data would be selected.
   *
   * @param trainingData Point/value pairs where that the sample should approximate, together with an error model (the uncertainty) at each point.
   */
  def marginalLikelihood(trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)]): Double = {
    require(trainingData.nonEmpty, "provide observations to calculate the marginal likelihood")
    GaussianProcess
      .marginalLikelihoodCalculation[Point[D], Value](cov.apply, mean.f, trainingData, outputDim)
  }

  /**
   * Discretizes the Gaussian Process at the given domain points. The
   * @param domain
   * @tparam DDomain
   * @return
   */
  def discretize[DDomain[DD] <: DiscreteDomain[DD]](domain: DDomain[D]): DiscreteGaussianProcess[D, DDomain, Value] = {

    require(domain.pointSet.numberOfPoints > 0)

    val meanField = DiscreteField[D, DDomain, Value](domain, domain.pointSet.points.toIndexedSeq.map(pt => mean(pt)))
    val pts = domain.pointSet.points.toIndexedSeq
    def newCov(i: PointId, j: PointId): DenseMatrix[Double] = {
      cov(pts(i.id), pts(j.id))
    }

    val discreteCov = DiscreteMatrixValuedPDKernel[D](domain, newCov, outputDim)
    new DiscreteGaussianProcess(meanField, discreteCov)
  }

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
    implicit
    vectorizer: Vectorizer[Value]
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
    val zeroField = Field[D, Value](EuclideanSpace[D], (p: Point[D]) => zeroVec)
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
      vectorizer.unvectorize((xstar(x) * K_inv) * fVec + vectorizer.vectorize(gp.mean(x)))
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
    gp.marginalLikelihood(trainingData)
  }

  /**
   * @todo The current implementation can be optimized as it inverts the data covariance matrix (that can be heavy for more than a few points). Instead an implementation
   *       with a Cholesky decomposition would be more efficient.
   */
  def marginalLikelihoodCalculation[A, Value](cov: (A, A) => DenseMatrix[Double],
                                              mean: A => Value,
                                              trainingData: IndexedSeq[(A, Value, MultivariateNormalDistribution)],
                                              outputDim: Int)(implicit vectorizer: Vectorizer[Value]): Double = {
    // below is the implementation according to Rassmussen Gaussian Processes, Chapter 5, page 113
    val (xs, ys, errorDistributions) = trainingData.unzip3
    val meanValues = xs.map(mean)
    val mVec = DiscreteField.vectorize[A, Value](meanValues)
    val yVec = DiscreteField.vectorize[A, Value](ys)
    val yVecZeroMean = yVec - mVec

    val Ky = DenseMatrix.zeros[Double](trainingData.size * outputDim, trainingData.size * outputDim)

    for ((xI, i) <- xs.zipWithIndex; (xJ, j) <- xs.zipWithIndex) {

      val covBlock = cov(xI, xJ)

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
    val const = outputDim * trainingData.length * 0.5 * math.log(math.Pi * 2)
    //det(KyInv) > 0, because Ky is PSD, therefore we can ignore the sign of logdet
    val margLikehood = ((yVecZeroMean.t * KyInv * yVecZeroMean) * -0.5) - (0.5 * logdet(Ky)._2) - const
    margLikehood
  }

}

object GaussianProcess1D {

  def apply[Value](mean: Field[_1D, Value], cov: MatrixValuedPDKernel[_1D])(
    implicit
    vectorizer: Vectorizer[Value]
  ): GaussianProcess[_1D, Value] = {
    new GaussianProcess[_1D, Value](mean, cov)
  }

  /**
   * Creates a new zero-mean Gaussian process with the given covariance function.
   */
  def apply[Value](
    cov: MatrixValuedPDKernel[_1D]
  )(implicit vectorizer: Vectorizer[Value]): GaussianProcess[_1D, Value] = {
    val zeroVec = vectorizer.unvectorize(DenseVector.zeros(vectorizer.dim))
    val zeroField = Field1D[Value](EuclideanSpace1D, (p: Point[_1D]) => zeroVec)
    GaussianProcess[_1D, Value](zeroField, cov)
  }

}

object GaussianProcess2D {

  def apply[Value](mean: Field[_2D, Value], cov: MatrixValuedPDKernel[_2D])(
    implicit
    vectorizer: Vectorizer[Value]
  ): GaussianProcess[_2D, Value] = {
    new GaussianProcess[_2D, Value](mean, cov)
  }

  /**
   * Creates a new zero-mean Gaussian process with the given covariance function.
   */
  def apply[Value](
    cov: MatrixValuedPDKernel[_2D]
  )(implicit vectorizer: Vectorizer[Value]): GaussianProcess[_2D, Value] = {
    val zeroVec = vectorizer.unvectorize(DenseVector.zeros(vectorizer.dim))
    val zeroField = Field2D[Value](EuclideanSpace2D, (p: Point[_2D]) => zeroVec)
    GaussianProcess[_2D, Value](zeroField, cov)
  }

}

object GaussianProcess3D {

  def apply[Value](mean: Field[_3D, Value], cov: MatrixValuedPDKernel[_3D])(
    implicit
    vectorizer: Vectorizer[Value]
  ): GaussianProcess[_3D, Value] = {
    new GaussianProcess[_3D, Value](mean, cov)
  }

  /**
   * Creates a new zero-mean Gaussian process with the given covariance function.
   */
  def apply[Value](
    cov: MatrixValuedPDKernel[_3D]
  )(implicit vectorizer: Vectorizer[Value]): GaussianProcess[_3D, Value] = {
    val zeroVec = vectorizer.unvectorize(DenseVector.zeros(vectorizer.dim))
    val zeroField = Field3D[Value](EuclideanSpace3D, (p: Point[_3D]) => zeroVec)
    GaussianProcess[_3D, Value](zeroField, cov)
  }

}
