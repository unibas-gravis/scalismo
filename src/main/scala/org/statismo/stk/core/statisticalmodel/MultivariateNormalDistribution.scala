package org.statismo.stk.core.statisticalmodel

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector, det}
import org.statismo.stk.core.geometry._

private[statisticalmodel] trait MultivariateNormalDistributionLike[V, M] {
  def mean: V

  def cov: M

  def dim: Int

  def principalComponents: Seq[(V, Double)]

  def pdf(x: V): Double

  def logpdf(x: V): Double

  def mahalanobisDistance(x: V): Double

  def drawSample(): V
}

case class MultivariateNormalDistribution(mean: DenseVector[Float], cov: DenseMatrix[Float])
  extends MultivariateNormalDistributionLike[DenseVector[Float], DenseMatrix[Float]] {

  require(cov.rows == cov.cols)
  require(mean.size == cov.rows)

  override val dim = mean.size


  private val covDouble = cov.map(_.toDouble)


  //private val covInvFloat = covInv.map(_.toFloat)
  private val SVD(uMat, sigma2s, utMat) = breeze.linalg.svd(covDouble)
  private val covDet = det(covDouble)
  private val sigma2spInv = sigma2s.map(s => if (s < 1e-6) 0 else 1.0 / s)
  private val sigmaMat = breeze.linalg.diag(sigma2s.map(math.sqrt))
  private val covInv = uMat * breeze.linalg.diag(sigma2spInv) * uMat.t


  /**
   * Returns a seq with the principal components and associated variance
   * @return
   */
  override def principalComponents: Seq[(DenseVector[Float], Double)] = {
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

  override def drawSample(): DenseVector[Float] = {

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

object NDimensionalNormalDistribution {
  def apply[D<: Dim : NDSpace](mean: Vector[D], principalComponents: Seq[(Vector[D], Float)]): NDimensionalNormalDistribution[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    require(principalComponents.length == dim)

    val cov: SquareMatrix[D] = {
      val d2 = {
        val data = Array.fill(dim*dim)(0.0f)
        for (i <- 0 until dim) data(i* dim + i) = principalComponents(i)._2
        SquareMatrix[D](data)
      }
      val u = SquareMatrix[D](principalComponents.map(_._1.data).flatten.toArray)
      u * d2 * u.t
    }
    NDimensionalNormalDistribution(mean, cov)
  }
}

case class NDimensionalNormalDistribution[D <: Dim : NDSpace](mean: Vector[D], cov: SquareMatrix[D])
  extends MultivariateNormalDistributionLike[Vector[D], SquareMatrix[D]] {

  private val impl = MultivariateNormalDistribution(mean.toBreezeVector, cov.toBreezeMatrix)

  override def pdf(x: Vector[D]): Double = impl.pdf(x.toBreezeVector)

  override def logpdf(x: Vector[D]): Double = impl.logpdf(x.toBreezeVector)

  override def dim: Int = implicitly[NDSpace[D]].dimensionality

  override def drawSample(): Vector[D] = Vector.fromBreezeVector(impl.drawSample())

  override def principalComponents: Seq[(Vector[D], Double)] = impl.principalComponents.map { case (v, d) => (Vector.fromBreezeVector(v), d)}

  override def mahalanobisDistance(x: Vector[D]): Double = impl.mahalanobisDistance(x.toBreezeVector)
}