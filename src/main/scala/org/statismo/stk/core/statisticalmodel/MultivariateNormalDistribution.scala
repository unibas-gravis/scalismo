package org.statismo.stk.core.statisticalmodel

import breeze.linalg.{det, DenseVector, DenseMatrix}
import org.statismo.stk.core.geometry._

class MultivariateNormalDistribution(val mean: DenseVector[Float], val cov: DenseMatrix[Float]) {

  require(cov.rows == cov.cols)
  require(mean.size == cov.rows)

  val dim = mean.size
  private val covDouble = cov.map(_.toDouble)


  //private val covInvFloat = covInv.map(_.toFloat)
  private val (uMat, sigma2s, utMat) = breeze.linalg.svd(covDouble)
  val covDet = det(covDouble)
  val sigma2spInv = sigma2s.map(s => if (s < 1e-6) 0 else 1.0 / s)
  val sigmaMat = breeze.linalg.diag(sigma2s.map(math.sqrt))
  private val covInv = uMat * breeze.linalg.diag(sigma2spInv) * uMat.t




  override def hashCode = mean.hashCode + cov.hashCode()
  override def equals(other: Any): Boolean = other match {
    case that: MultivariateNormalDistribution => {
      that.canEqual(this) && this.mean == that.mean && this.cov == that.cov
    }
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MultivariateNormalDistribution]


  /**
   * Returns a seq with the principal components and associated variance
   * @return
   */
  def principalComponents : Seq[(DenseVector[Float], Double)] = {
    for (i <- 0 until uMat.cols) yield {
      (uMat(::, i).toDenseVector.map(_.toFloat), sigma2s(i))
    }
  }

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
    val x0 = (x - mean).map(_.toDouble)
    math.sqrt(x0 dot (covInv * x0))
  }

  def drawSample : DenseVector[Float] = {

    val normalSamples = for (i <- 0 until dim) yield breeze.stats.distributions.Gaussian(0, 1).draw()
    val u = DenseVector[Double](normalSamples.toArray)
    //mean + (L * u).map(_.toFloat) // a random sample

    mean + (uMat * (sigmaMat * u)).map(_.toFloat)
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

case class MVNormalForPoint[D <: Dim](val pt: Point[D], _mean: Vector[D], _cov: MatrixNxN[D]) extends MultivariateNormalDistribution(_mean.toBreezeVector, _cov.toBreezeMatrix) {
  def pdf(x: Vector[D]): Double = pdf(x.toBreezeVector)
  def logpdf(x: Vector[D]): Double = logpdf(x.toBreezeVector)
}