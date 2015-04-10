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
import scalismo.common.{ RealSpace, VectorField, DiscreteDomain, DiscreteVectorField }
import scalismo.geometry._
import scalismo.kernels.{ MatrixValuedPDKernel, Kernel, DiscreteMatrixValuedPDKernel }
import scalismo.mesh.kdtree.KDTreeMap

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

    val sampleVec = mvNormal.sample()

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

  /**
   * Interpolates discrete Gaussian process to have a new, continuous representation as a [[DiscreteLowRankGaussianProcess]],
   * using nearest neigbor interpolation (for both mean and covariance function)
   */
  def interpolateNearestNeighbor: GaussianProcess[D, DO] = {

    val meanDiscreteGp = this.mean
    val kdTreeMap = KDTreeMap.fromSeq(domain.pointsWithId.toIndexedSeq)

    def closestPt(pt: Point[D]): (Point[D], Int) = {
      val closestPts = kdTreeMap.findNearest(pt, n = 1)
      val (closestPt, closestPtId) = closestPts(0)
      (closestPt, closestPtId)
    }

    val newDomain = RealSpace[D]
    def meanFun(pt: Point[D]): Vector[DO] = {
      val (_, closestPtId) = closestPt(pt)
      meanDiscreteGp(closestPtId)
    }

    val newCov = new MatrixValuedPDKernel[D, DO] {
      override val domain = newDomain

      override def k(pt1: Point[D], pt2: Point[D]): SquareMatrix[DO] = {
        val (_, closestPtId1) = closestPt(pt1)
        val (_, closestPtId2) = closestPt(pt2)
        cov(closestPtId1, closestPtId2)
      }
    }

    GaussianProcess(VectorField(newDomain, meanFun), newCov)

  }

  /**
   * Discrete version of [[LowRankGaussianProcess.project(IndexedSeq[(Point[D], Vector[DO])], Double)]]
   */

  def project(s: DiscreteVectorField[D, DO]): DiscreteVectorField[D, DO] = {

    val sigma2 = 1e-5f // regularization weight to avoid numerical problems
    val noiseDist = NDimensionalNormalDistribution(Vector.zeros[DO], SquareMatrix.eye[DO] * sigma2)
    val td = s.values.zipWithIndex.map { case (v, id) => (id, v, noiseDist) }.toIndexedSeq
    DiscreteGaussianProcess.regression(this, td).mean

  }

  /**
   * Returns the probability density of the given instance
   */
  def pdf(instance: DiscreteVectorField[D, DO]): Double = {
    val mvnormal = MultivariateNormalDistribution(mean.asBreezeVector, cov.asBreezeMatrix)
    val instvec = instance.asBreezeVector
    mvnormal.pdf(instvec)
  }

}

object DiscreteGaussianProcess {

  def apply[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace](mean: DiscreteVectorField[D, DO], cov: DiscreteMatrixValuedPDKernel[D, DO]) = {
    new DiscreteGaussianProcess[D, DO](mean, cov)
  }

  def apply[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace](domain: DiscreteDomain[D], gp: GaussianProcess[D, DO]) = {
    val domainPoints = domain.points.toIndexedSeq

    val discreteMean = DiscreteVectorField[D, DO](domain, domainPoints.map(pt => gp.mean(pt)))

    val k = (i: Int, j: Int) => gp.cov(domainPoints(i), domainPoints(j))
    val discreteCov = DiscreteMatrixValuedPDKernel(domain, k)

    new DiscreteGaussianProcess[D, DO](discreteMean, discreteCov)
  }

  def regression[D <: Dim: NDSpace: CanBound, DO <: Dim: NDSpace](discreteGp: DiscreteGaussianProcess[D, DO],
    trainingData: IndexedSeq[(Int, Vector[DO], NDimensionalNormalDistribution[DO])]): DiscreteGaussianProcess[D, DO] = {

    // TODO, this is somehow a hack to reuse the code written for the general GP regression. We should think if that has disadvantages
    // TODO We should think whether we can do it in  a conceptually more clean way.

    val domainPoints = discreteGp.domain.points.toIndexedSeq
    val gp = discreteGp.interpolateNearestNeighbor
    val tdForGp = trainingData.map({ case (id, vec, error) => (domainPoints(id), vec, error) })
    val posterior = GaussianProcess.regression(gp, tdForGp)

    DiscreteGaussianProcess(discreteGp.domain, gp)
  }
}