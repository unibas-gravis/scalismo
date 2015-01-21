package org.statismo.stk.core
package statisticalmodel

import breeze.linalg.svd.SVD
import breeze.linalg.{*, DenseVector, DenseMatrix}
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.common.ImmutableLRU
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.geometry.{Point, Vector, Dim}



class GaussianProcess[D <: Dim : NDSpace] protected (val domain : Domain[D], val mean : Point[D] => Vector[D], val cov : MatrixValuedPDKernel[D, D]) {

  protected[this] val dimOps : NDSpace[D] = implicitly[NDSpace[D]]

  def outputDimensionality = dimOps.dimensionality

  def sampleAtPoints(pts : Seq[Point[D]]) : Seq[(Point[D], Vector[D])] = {
    val K = Kernel.computeKernelMatrix(pts, cov).map(_.toDouble)


    // TODO using the svd is slightly inefficient, but with the current version of breeze, the cholesky decomposition does not seem to work
    val SVD(u, s, _) = breeze.linalg.svd(K)
    val L = u.copy
    for (i <- 0 until s.size) {
      L(::, i) := u(::, i) * Math.sqrt(s(i))
    }
       val r = breeze.stats.distributions.Gaussian(0, 1)
    val nGaussians = for (i <- 0 until pts.size * outputDimensionality) yield r.draw()
    val v =DenseVector(nGaussians.toArray)
    val sampleVec = L * v
    val vecs = sampleVec.toArray.grouped(outputDimensionality)
      .map(data => Vector[D](data.map(_.toFloat)))
      .toSeq
    pts zip vecs
  }

  /**
   * Compute the marginal distribution for the given point
   */
  def marginal(pt: Point[D]): NDimensionalNormalDistribution[D] = NDimensionalNormalDistribution(mean(pt), cov(pt, pt))
}




object GaussianProcess {

  def apply[D <: Dim : NDSpace](domain : Domain[D],  mean : Point[D] => Vector[D], cov : MatrixValuedPDKernel[D, D]) = {
    new GaussianProcess[D](domain, mean, cov)
  }

  // Gaussian process regression for a low rank gaussian process
  // Note that this implementation is literally the same as the one for the specializedLowRankGaussian process. The difference is just the return type. 
  // TODO maybe the implementations can be joined.
  def regression[D <: Dim: NDSpace](gp: LowRankGaussianProcess[D, D], trainingData: IndexedSeq[(Point[D], Vector[D])], sigma2: Double, meanOnly: Boolean = false): LowRankGaussianProcess[D, D] = {
    val trainingDataWithNoise = trainingData.map { case (x, y) => (x, y, sigma2)}
    regression(gp, trainingDataWithNoise, meanOnly)
  }

  def regression[D <: Dim: NDSpace](gp: LowRankGaussianProcess[D, D], trainingData: IndexedSeq[(Point[D], Vector[D], Double)], meanOnly: Boolean = false): LowRankGaussianProcess[D, D] = {
    val (lambdas, phis) = gp.eigenPairs.unzip
    val outputDim = gp.outputDimensionality


    val dim = implicitly[NDSpace[D]].dimensionality
    def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

    val (xs, ys, sigma2s) = trainingData.unzip3

    val yVec = flatten(ys)
    val meanValues = xs.map(gp.mean)
    val mVec = flatten(meanValues)

    val Q = DenseMatrix.zeros[Double](trainingData.size * dim, phis.size)
    for ((x_i, i) <- xs.zipWithIndex; (phi_j, j) <- phis.zipWithIndex) {
      Q(i * dim until i * dim + dim, j) := phi_j(x_i).toBreezeVector.map(_.toDouble) * math.sqrt(lambdas(j))
    }

    // compute Q^TL where L is a diagonal matrix that contains the inverse of the sigmas in the diagonal.
    // As there is only one sigma for each point (but the point has dim components) we need
    // to correct the index for sigma
    val QtL = Q.t.copy
    val sigma2sInv = sigma2s.map { sigma2 =>
      val divisor = math.max(1e-8, sigma2)
      1.0 / divisor
    }
    for (i <- 0 until QtL.cols) {
      QtL(::, i) *= sigma2sInv(i / dim)
    }

    val M = QtL * Q + DenseMatrix.eye[Double](phis.size)
    val Minv = breeze.linalg.pinv(M)


    val mean_coeffs = (Minv * QtL).map(_.toFloat) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    if (meanOnly == true) {
      val emptyEigenPairs = IndexedSeq[(Float, Point[D] => Vector[D])]()
      new LowRankGaussianProcess[D, D](gp.domain, mean_p, emptyEigenPairs)

    } else {
      val D = breeze.linalg.diag(DenseVector(lambdas.map(math.sqrt(_)).toArray))
      val Sigma = D * Minv * D
      val SVD(innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
      val innerU = innerUDbl.map(_.toFloat)
      @volatile
      var phisAtXCache = ImmutableLRU[Point[D], DenseMatrix[Float]](1000)

      def phip(i: Int)(x: Point[D]): Vector[D] = { // should be phi_p but _ is treated as partial function
        val (maybePhisAtX, newPhisAtXCache) = phisAtXCache.get(x)
        val phisAtX = maybePhisAtX.getOrElse {
          val newPhisAtX = {
            val innerPhisAtx = DenseMatrix.zeros[Float](outputDim, gp.rank)
            var j = 0;
            while (j < phis.size) {
              val phi_j = phis(j)
              innerPhisAtx(0 until outputDim, j) := phi_j(x).toBreezeVector
              j += 1
            }
            innerPhisAtx
          }
          phisAtXCache = (phisAtXCache + ((x, newPhisAtX)))._2 // ignore evicted key
          newPhisAtX
        }
        val vec = phisAtX * innerU(::, i)
        Vector[D](vec.data)
      }

      val phis_p = for (i <- 0 until phis.size) yield ((x : Point[D]) => phip(i)(x))
      val lambdas_p = innerD2.toArray.map(_.toFloat).toIndexedSeq
      new LowRankGaussianProcess[D, D](gp.domain, mean_p, lambdas_p.zip(phis_p))
    }
  }





}
