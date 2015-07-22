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
import breeze.linalg.{ DenseMatrix, DenseVector, det }
import scalismo.geometry._

import scala.util.Try

private[statisticalmodel] trait MultivariateNormalDistributionLike[V, M] {
  def mean: V

  def cov: M

  def dim: Int

  def principalComponents: Seq[(V, Double)]

  def pdf(x: V): Double

  def logpdf(x: V): Double

  def mahalanobisDistance(x: V): Double

  def sample(): V
}

case class MultivariateNormalDistribution(mean: DenseVector[Float], cov: DenseMatrix[Float])
    extends MultivariateNormalDistributionLike[DenseVector[Float], DenseMatrix[Float]] {

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
  override def principalComponents: Seq[(DenseVector[Float], Double)] = {
    val SVD(uMat, sigma2s, utMat) = breeze.linalg.svd.reduced(cov.map(_.toDouble))
    val sigma2spInv = sigma2s.map(s => if (s < 1e-6) 0 else 1.0 / s)
    val sigmaMat = breeze.linalg.diag(sigma2s.map(math.sqrt))
    val covInv = uMat * breeze.linalg.diag(sigma2spInv) * uMat.t

    for (i <- 0 until uMat.cols) yield {
      (uMat(::, i).toDenseVector.map(_.toFloat), sigma2s(i))
    }
  }

  override def pdf(x: DenseVector[Float]) = {
    if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
    val normFactor = math.pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / math.sqrt(covDet)

    val x0 = (x - mean).map(_.toDouble)
    val exponent = -0.5f * x0.dot(covInv * x0)
    normFactor * math.exp(exponent)
  }

  override def logpdf(x: DenseVector[Float]) = {
    if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
    val normFactor = math.pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / math.sqrt(covDet)

    val x0 = (x - mean).map(_.toDouble)
    val exponent = -0.5f * x0.dot(covInv * x0)
    math.log(normFactor) + exponent

  }

  override def mahalanobisDistance(x: DenseVector[Float]): Double = {
    val x0 = (x - mean).map(_.toDouble)
    math.sqrt(x0 dot (covInv * x0))
  }

  override def sample(): DenseVector[Float] = {

    val normalSamples = for (i <- 0 until dim) yield breeze.stats.distributions.Gaussian(0, 1).draw()
    val u = DenseVector[Double](normalSamples.toArray)

    mean + (root * u).map(_.toFloat)
  }

  private def regularizedCholesky(regWeight: Double): Try[DenseMatrix[Double]] = {

    // we regularize the covariance matrix before doing a cholesky decomposition
    // to avoid numerical errors
    val covReg = cov.map(_.toDouble)
    for (i <- 0 until cov.cols) {
      covReg(i, i) += regWeight
    }

    Try {
      breeze.linalg.cholesky(covReg)
    }
  }

}

object MultivariateNormalDistribution {

  def estimateFromData(samples: Seq[DenseVector[Float]]): MultivariateNormalDistribution = {

    val numSamples = samples.length
    require(numSamples > 0)

    val sampleDim = samples(0).length
    require(samples.forall(s => s.length == sampleDim))

    val zeroVec = DenseVector.zeros[Float](sampleDim)
    val mean = samples.foldLeft(zeroVec)((acc, s) => acc + s) * (1f / numSamples)

    val zeroMatrix = DenseMatrix.zeros[Float](sampleDim, sampleDim)
    def outer(v1: DenseVector[Float], v2: DenseVector[Float]) = v1.toDenseMatrix.t * v2.toDenseMatrix

    val normalizer = if (numSamples == 1) 1 else numSamples - 1
    val cov = samples.foldLeft(zeroMatrix)((acc, s) => acc + outer(s - mean, s - mean)) * (1f / normalizer)
    new MultivariateNormalDistribution(mean, cov)
  }

}

object NDimensionalNormalDistribution {
  def apply[D <: Dim: NDSpace](mean: Vector[D], principalComponents: Seq[(Vector[D], Float)]): NDimensionalNormalDistribution[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(principalComponents.length == dim)

    val cov: SquareMatrix[D] = {
      val d2 = {
        val data = Array.fill(dim * dim)(0.0f)
        for (i <- 0 until dim) data(i * dim + i) = principalComponents(i)._2
        SquareMatrix[D](data)
      }
      val u = SquareMatrix[D](principalComponents.map(_._1.toArray).flatten.toArray)
      u * d2 * u.t
    }
    NDimensionalNormalDistribution(mean, cov)
  }
}

case class NDimensionalNormalDistribution[D <: Dim: NDSpace](mean: Vector[D], cov: SquareMatrix[D])
    extends MultivariateNormalDistributionLike[Vector[D], SquareMatrix[D]] {

  private val impl = MultivariateNormalDistribution(mean.toBreezeVector, cov.toBreezeMatrix)

  override def pdf(x: Vector[D]): Double = impl.pdf(x.toBreezeVector)

  override def logpdf(x: Vector[D]): Double = impl.logpdf(x.toBreezeVector)

  override def dim: Int = implicitly[NDSpace[D]].dimensionality

  override def sample(): Vector[D] = Vector.fromBreezeVector(impl.sample())

  override def principalComponents: Seq[(Vector[D], Double)] = impl.principalComponents.map { case (v, d) => (Vector.fromBreezeVector(v), d) }

  override def mahalanobisDistance(x: Vector[D]): Double = impl.mahalanobisDistance(x.toBreezeVector)
}