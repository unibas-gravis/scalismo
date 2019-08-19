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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry._
import scalismo.kernels.{DiscreteMatrixValuedPDKernel, MatrixValuedPDKernel}
import scalismo.numerics.PivotedCholesky
import scalismo.numerics.PivotedCholesky.RelativeTolerance
import scalismo.utils.Random

/**
 * A representation of a gaussian process, which is only defined on a discrete domain.
 * While this is technically similar to a MultivariateNormalDistribution, we highlight with this
 * class that we represent (discrete) functions, defined on the given domain.
 */
class DiscreteGaussianProcess[D: NDSpace, +DDomain <: DiscreteDomain[D], Value] private[scalismo] (val mean: DiscreteField[D, DDomain, Value],
    val cov: DiscreteMatrixValuedPDKernel[D])(implicit val vectorizer: Vectorizer[Value]) {
  self =>

  require(mean.domain == cov.domain)

  val domain = mean.domain

  val outputDim = vectorizer.dim

  def sample()(implicit rand: Random): DiscreteField[D, DDomain, Value] = {
    // define the mean and kernel matrix for the given points and construct the
    // corresponding MV Normal distribution, from which we then sample

    val mu = DiscreteField.vectorize[D, Value](mean.data)
    val K = cov.asBreezeMatrix

    val mvNormal = MultivariateNormalDistribution(mu, K)

    val sampleVec = mvNormal.sample

    // The sample is a vector. We convert it back to a discreteVectorField.
    DiscreteField.createFromDenseVector[D, DDomain, Value](domain, sampleVec)
  }

  /**
   * The marginal distribution at a given (single) point, specified by the pointId.
   */
  def marginal(pointId: PointId) = {
    MultivariateNormalDistribution(vectorizer.vectorize(mean(pointId)), cov(pointId, pointId))
  }

  /**
   * The marginal distribution for the points specified by the given point ids.
   * Note that this is again a DiscreteGaussianProcess.
   */
  def marginal(pointIds: Seq[PointId])(implicit domainCreator: UnstructuredPointsDomain.Create[D]): DiscreteGaussianProcess[D, UnstructuredPointsDomain[D], Value] = {
    val domainPts = domain.points.toIndexedSeq

    val newPts = pointIds.map(pointId => domainPts(pointId.id)).toIndexedSeq
    val newDomain = domainCreator.create(newPts)

    val newMean = DiscreteField[D, UnstructuredPointsDomain[D], Value](newDomain, pointIds.toIndexedSeq.map(id => mean(id)))
    val newCov = (i: PointId, j: PointId) => {
      cov(pointIds(i.id), pointIds(j.id))
    }
    val newDiscreteCov = DiscreteMatrixValuedPDKernel(newDomain, newCov, outputDim)

    new DiscreteGaussianProcess(newMean, newDiscreteCov)
  }

  /**
   * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianProcess]],
   * using nearest neighbor interpolation (for both mean and covariance function)
   */
  @deprecated("please use the [[interpolate]] method with a [[NearestNeighborInterpolator]] instead", "0.16")
  def interpolateNearestNeighbor: GaussianProcess[D, Value] = {

    val meanDiscreteGp = this.mean

    val newDomain = RealSpace[D]
    def meanFun(pt: Point[D]): Value = {
      val closestPtId = domain.findClosestPoint(pt).id
      meanDiscreteGp(closestPtId)
    }

    val newCov = new MatrixValuedPDKernel[D] {
      override val domain = newDomain

      override def k(pt1: Point[D], pt2: Point[D]): DenseMatrix[Double] = {
        val closestPtId1 = self.domain.findClosestPoint(pt1).id
        val closestPtId2 = self.domain.findClosestPoint(pt2).id
        cov(closestPtId1, closestPtId2)
      }

      override def outputDim = self.outputDim
    }

    GaussianProcess(Field[D, Value](newDomain, meanFun), newCov)

  }

  def interpolate(interpolator : FieldInterpolator[D, DDomain, Value]) : GaussianProcess[D, Value] = {

    // We know how to interpolate DiscreteLowRankGaussianProcesses, but
    // not this more generic type of DiscreteGP. Since we are sure that our
    // cov matrix is always relatively small (it has to fit in memory)
    // we can do the trick and do a pivoted cholesky, create a
    // DiscreteLowRankGP and interpolate that.

    val (basis, scale) = PivotedCholesky.computeApproximateEig(
      cov.asBreezeMatrix,
      RelativeTolerance(0.0)
    )

    val nBasisFunctions = basis.cols

    val klBasis: DiscreteLowRankGaussianProcess.KLBasis[D, DDomain, Value] = for (i <- 0 until nBasisFunctions) yield {
      val discreteEV = DiscreteField.createFromDenseVector[D, DDomain, Value](domain, basis(::, i))
      DiscreteLowRankGaussianProcess.Eigenpair(scale(i), discreteEV)
    }

    val dgp = DiscreteLowRankGaussianProcess[D, DDomain, Value](mean, klBasis)
    dgp.interpolate(interpolator)
  }


  /**
   * Discrete version of [[LowRankGaussianProcess.project(IndexedSeq[(Point[D], Vector[DO])], Double)]]
   */

  def project(s: DiscreteField[D, DiscreteDomain[D], Value]): DiscreteField[D, DDomain, Value] = {

    val sigma2 = 1e-5 // regularization weight to avoid numerical problems
    val noiseDist = MultivariateNormalDistribution(DenseVector.zeros[Double](outputDim), DenseMatrix.eye[Double](outputDim) * sigma2)
    val td = s.values.zipWithIndex.map { case (v, id) => (id, v, noiseDist) }.toIndexedSeq
    DiscreteGaussianProcess.regression(this, td).mean

  }

  /**
   * Returns the probability density of the given instance
   */
  def pdf(instance: DiscreteField[D, DiscreteDomain[D], Value]): Double = {
    val mvnormal = MultivariateNormalDistribution(DiscreteField.vectorize[D, Value](mean.data), cov.asBreezeMatrix)
    val instvec = DiscreteField.vectorize[D, Value](instance.data)
    mvnormal.pdf(instvec)
  }

  /**
   * Returns the log of the probability density of the given instance
   *
   * If you are interested in ordinal comparisons of PDFs, use this as it is numerically more stable
   */
  def logpdf(instance: DiscreteField[D, DiscreteDomain[D], Value]): Double = {
    val mvnormal = MultivariateNormalDistribution(DiscreteField.vectorize[D, Value](mean.data), cov.asBreezeMatrix)
    val instvec = DiscreteField.vectorize[D, Value](instance.data)
    mvnormal.logpdf(instvec)
  }

}

object DiscreteGaussianProcess {

  def apply[D: NDSpace, DDomain <: DiscreteDomain[D], Value](mean: DiscreteField[D, DDomain, Value], cov: DiscreteMatrixValuedPDKernel[D])(implicit vectorizer: Vectorizer[Value]): DiscreteGaussianProcess[D, DDomain, Value] = {
    new DiscreteGaussianProcess[D, DDomain, Value](mean, cov)
  }

  def apply[D: NDSpace, DDomain <: DiscreteDomain[D], Value](domain: DDomain, gp: GaussianProcess[D, Value])(implicit vectorizer: Vectorizer[Value]): DiscreteGaussianProcess[D, DDomain, Value] = {
    val domainPoints = domain.points.toIndexedSeq

    val discreteMean = DiscreteField[D, DDomain, Value](domain, domainPoints.map(pt => gp.mean(pt)))

    val k = (i: PointId, j: PointId) => gp.cov(domainPoints(i.id), domainPoints(j.id))
    val discreteCov = DiscreteMatrixValuedPDKernel(domain, k, gp.outputDim)

    new DiscreteGaussianProcess[D, DDomain, Value](discreteMean, discreteCov)
  }

  def regression[D: NDSpace, DDomain <: DiscreteDomain[D], Value](discreteGp: DiscreteGaussianProcess[D, DDomain, Value], trainingData: IndexedSeq[(Int, Value, MultivariateNormalDistribution)])(implicit vectorizer: Vectorizer[Value]): DiscreteGaussianProcess[D, DDomain, Value] = {

    // TODO, this is somehow a hack to reuse the code written for the general GP regression. We should think if that has disadvantages
    // TODO We should think whether we can do it in  a conceptually more clean way.

    val domainPoints = discreteGp.domain.points.toIndexedSeq
    val gp = discreteGp.interpolate(NearestNeighborInterpolator())
    val tdForGp = trainingData.map({ case (id, vec, error) => (domainPoints(id), vec, error) })
    val posterior = GaussianProcess.regression(gp, tdForGp)

    DiscreteGaussianProcess(discreteGp.domain, posterior)
  }
}