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
import breeze.linalg._
import breeze.stats.distributions.Gaussian
import scalismo.geometry.Vector
import scalismo.geometry._
import scalismo.utils.Random

import scala.util.Try

private[statisticalmodel] trait MultivariateNormalDistributionLike[V, M] {
  def mean: V

  def cov: M

  def dim: Int

  def principalComponents: Seq[(V, Double)]

  def pdf(x: V): Double

  def logpdf(x: V): Double

  def mahalanobisDistance(x: V): Double

  def sample()(implicit rand: Random): V
}

case class MultivariateNormalDistribution(mean: DenseVector[Double], cov: DenseMatrix[Double])
    extends MultivariateNormalDistributionLike[DenseVector[Double], DenseMatrix[Double]] {

  require(cov.rows == cov.cols)
  require(mean.size == cov.rows)

  override val dim = mean.size

  private lazy val covInv = breeze.linalg.pinv(cov.map(_.toDouble))
  private lazy val covDet = det(cov.map(_.toDouble))

  // The root of the covariance matrix is precomputed for efficient sampling.
  // The cholesky sometimes fails for ill conditioned matrices. We increase
  // the regularization weight until it converges
  private lazy val root = {
    Iterator.iterate(1e-10)(w => w * 2)
      .map(w => regularizedCholesky(w))
      .dropWhile(_.isFailure) // we drop the result if the cholesky fails
      .next().get
  }

  /**
   * Returns a seq with the principal components and associated variance
   * @return
   */
  override def principalComponents: Seq[(DenseVector[Double], Double)] = {
    val SVD(uMat, sigma2s, utMat) = breeze.linalg.svd.reduced(cov.map(_.toDouble))
    val sigma2spInv = sigma2s.map(s => if (s < 1e-6) 0 else 1.0 / s)
    val sigmaMat = breeze.linalg.diag(sigma2s.map(math.sqrt))
    val covInv = uMat * breeze.linalg.diag(sigma2spInv) * uMat.t

    for (i <- 0 until uMat.cols) yield {
      (uMat(::, i).toDenseVector, sigma2s(i))
    }
  }

  override def pdf(x: DenseVector[Double]) = {
    if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
    val normFactor = math.pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / math.sqrt(covDet + 1e-10)

    val x0 = (x - mean)
    val exponent = -0.5f * x0.dot(covInv * x0)
    normFactor * math.exp(exponent)
  }

  override def logpdf(x: DenseVector[Double]) = {
    if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
    val normFactor = math.pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / math.sqrt(covDet + 1e-10)

    val x0 = (x - mean).map(_.toDouble)
    val exponent = -0.5f * x0.dot(covInv * x0)
    math.log(normFactor) + exponent

  }

  override def mahalanobisDistance(x: DenseVector[Double]): Double = {
    val x0 = (x - mean)
    math.sqrt(x0 dot (covInv * x0))
  }

  override def sample()(implicit rand: Random): DenseVector[Double] = {
    val standardNormal = Gaussian(0, 1)(rand.breezeRandBasis)
    val normalSamples = standardNormal.sample(dim)
    val u = DenseVector[Double](normalSamples.toArray)

    mean + (root * u)
  }

  private def regularizedCholesky(regWeight: Double): Try[DenseMatrix[Double]] = {

    // we regularize the covariance matrix before doing a cholesky decomposition
    // to avoid numerical errors
    val covReg = cov.copy
    for (i <- 0 until covReg.cols) {
      covReg(i, i) += regWeight
    }

    Try {
      breeze.linalg.cholesky(covReg)
    }
  }

  def marginal(subspace: IndexedSeq[Int]): MultivariateNormalDistribution = {
    val redMean = mean(subspace).toDenseVector
    val redCov = cov(subspace, subspace).toDenseMatrix
    MultivariateNormalDistribution(redMean, redCov)
  }

  def conditional(observations: IndexedSeq[(Int, Double)]): MultivariateNormalDistribution = {

    val (obsIdx: IndexedSeq[Int], obsVals: IndexedSeq[Double]) = observations.unzip
    val unknownIdx = (0 until mean.length).filter(e => !obsIdx.contains(e))

    val meanUn = mean(unknownIdx)
    val meanObs = mean(obsIdx)

    val covUnUn = cov(unknownIdx, unknownIdx)
    val covUnObs = cov(unknownIdx, obsIdx)
    val covObsUn = cov(obsIdx, unknownIdx)
    val covObsObs = cov(obsIdx, obsIdx)

    val diff = DenseVector(obsVals.toArray) - meanObs
    val mprod = covUnObs * inv(covObsObs.toDenseMatrix)
    val newMean = meanUn + mprod * diff

    val newCov = covUnUn - mprod * covObsUn

    MultivariateNormalDistribution(newMean.toDenseVector, newCov.toDenseMatrix)
  }

}

object MultivariateNormalDistribution {

  def apply(mean: DenseVector[Double], principalComponents: Seq[(DenseVector[Double], Double)]): MultivariateNormalDistribution = {

    val dim = mean.length
    require(principalComponents.length == dim)

    val cov: DenseMatrix[Double] = {
      val d2 = diag(DenseVector[Double](principalComponents.map(_._2).toArray))

      // stack pcs
      val u = principalComponents.tail.foldLeft(principalComponents.head._1.toDenseMatrix) {
        case (m: DenseMatrix[Double], pc: (DenseVector[Double], Double)) =>
          DenseMatrix.vertcat(m, pc._1.toDenseMatrix)
      }

      u * d2 * u.t
    }
    MultivariateNormalDistribution(mean, cov)
  }

  def estimateFromData(samples: Seq[DenseVector[Double]]): MultivariateNormalDistribution = {

    val numSamples = samples.length
    require(numSamples > 0)

    val sampleDim = samples(0).length
    require(samples.forall(s => s.length == sampleDim))

    val zeroVec = DenseVector.zeros[Double](sampleDim)
    val mean = samples.foldLeft(zeroVec)((acc, s) => acc + s) * (1.0 / numSamples)

    val zeroMatrix = DenseMatrix.zeros[Double](sampleDim, sampleDim)
    def outer(v1: DenseVector[Double], v2: DenseVector[Double]) = v1.toDenseMatrix.t * v2.toDenseMatrix

    val normalizer = if (numSamples == 1) 1 else numSamples - 1
    val cov = samples.foldLeft(zeroMatrix)((acc, s) => acc + outer(s - mean, s - mean)) * (1.0 / normalizer)
    new MultivariateNormalDistribution(mean, cov)
  }

}

@deprecated("Please use MultivariateNormalDistribution instead. This object wil be removed in future versions.", "0.13.0")
object NDimensionalNormalDistribution {
  def apply[D <: Dim: NDSpace](mean: Vector[D], principalComponents: Seq[(Vector[D], Double)]): NDimensionalNormalDistribution[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(principalComponents.length == dim)

    val cov: SquareMatrix[D] = {
      val d2 = {
        val data = Array.fill(dim * dim)(0.0)
        for (i <- 0 until dim) data(i * dim + i) = principalComponents(i)._2
        SquareMatrix[D](data)
      }
      val u = SquareMatrix[D](principalComponents.flatMap(_._1.toArray).toArray)
      u * d2 * u.t
    }
    NDimensionalNormalDistribution(mean, cov)
  }
}

@deprecated("Please use MultivariateNormalDistribution instead. This class wil be removed in future versions.", "0.13.0")
case class NDimensionalNormalDistribution[D <: Dim: NDSpace](mean: Vector[D], cov: SquareMatrix[D])
    extends MultivariateNormalDistributionLike[Vector[D], SquareMatrix[D]] {

  private val impl = MultivariateNormalDistribution(mean.toBreezeVector, cov.toBreezeMatrix)

  override def pdf(x: Vector[D]): Double = impl.pdf(x.toBreezeVector)

  override def logpdf(x: Vector[D]): Double = impl.logpdf(x.toBreezeVector)

  override def dim: Int = implicitly[NDSpace[D]].dimensionality

  override def sample()(implicit rand: Random): Vector[D] = Vector.fromBreezeVector(impl.sample)

  override def principalComponents: Seq[(Vector[D], Double)] = impl.principalComponents.map { case (v, d) => (Vector.fromBreezeVector(v), d) }

  override def mahalanobisDistance(x: Vector[D]): Double = impl.mahalanobisDistance(x.toBreezeVector)

  def toMultivariateNormalDistribution: MultivariateNormalDistribution = impl
}