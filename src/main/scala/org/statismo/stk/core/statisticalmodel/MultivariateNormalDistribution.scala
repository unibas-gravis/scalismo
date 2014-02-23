package org.statismo.stk.core.statisticalmodel

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg.LinearAlgebra
import org.statismo.stk.core.geometry._

class MultivariateNormalDistribution(val mean: DenseVector[Float], val cov: DenseMatrix[Float]) {

  require(cov.rows == cov.cols)
  require(mean.size == cov.rows)

  val dim = mean.size

  val covInv = LinearAlgebra.pinv(cov)
  private val covInvFloat = covInv.map(_.toFloat)
  val L = breeze.linalg.cholesky(cov.map(_.toDouble) + DenseMatrix.eye[Double](dim) * 1e-6) // lower diagonal matrix
  val covDet = LinearAlgebra.det(cov)

  def pdf(x: DenseVector[Float]) = {
    if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
    val normFactor = math.pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / math.sqrt(covDet)

    val x0 = (x - mean).map(_.toDouble)
    val exponent = -0.5f * x0.dot(covInv * x0)
    normFactor * math.exp(exponent)
  }

  def logpdf(x: DenseVector[Float]) = {
    if (x.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${x.size} should be $dim)")
    val normFactor = math.pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / math.sqrt(covDet)

    val x0 = (x - mean).map(_.toDouble)
    val exponent = -0.5f * x0.dot(covInv * x0)
    math.log(normFactor) + exponent

  }

  def mahalanobisDistance(x: DenseVector[Float]): Double = {
    val x0 = (x - mean)
    math.sqrt(x0 dot (covInvFloat * x0))
  }

  def drawSample : DenseVector[Float] = {

    val normalSamples = for (i <- 0 until dim) yield breeze.stats.distributions.Gaussian(0, 1).draw()
    val u = DenseVector[Double](normalSamples.toArray)
    mean + (L * u).map(_.toFloat) // a random sample
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
    
    val cov = samples.foldLeft(zeroMatrix)((acc, s) => acc + outer(s - mean, s - mean)) * (1f / (numSamples - 1))
    new MultivariateNormalDistribution(mean, cov)
  }

}

case class MVNormalForPoint[D <: Dim: DimTraits](val pt: Point[D], _mean: Vector[D], _cov: MatrixNxN[D]) extends MultivariateNormalDistribution(_mean.toBreezeVector, _cov.toBreezeMatrix) {
  def pdf(x: Vector[D]): Double = pdf(x.toBreezeVector)
  def logpdf(x: Vector[D]): Double = logpdf(x.toBreezeVector)
}