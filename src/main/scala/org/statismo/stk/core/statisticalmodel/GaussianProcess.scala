package org.statismo.stk.core
package statisticalmodel

import breeze.linalg.{ DenseVector, DenseMatrix}
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.common.ImmutableLRU
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.geometry.{Point, Vector, Dim}


trait GaussianProcess[D <: Dim] {

  protected[this] val dimTraits : DimTraits[D]

  def outputDimensionality = dimTraits.dimensionality
  val domain: Domain[D]
  val mean: Point[D] => Vector[D]
  val cov: MatrixValuedPDKernel[D, D]
  def rank : Int
  def marginal(pt: Point[D]): MVNormalForPoint[D]
  def jacobian(p: DenseVector[Float]) : Point[D] => DenseMatrix[Float]
  def instance(alpha: DenseVector[Float]): Point[D] => Vector[D]
}




object GaussianProcess {


  // Gaussian process regression for a low rank gaussian process
  // Note that this implementation is literally the same as the one for the specializedLowRankGaussian process. The difference is just the return type. 
  // TODO maybe the implementations can be joined.
  def regression[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], trainingData: IndexedSeq[(Point[D], Vector[D])], sigma2: Double, meanOnly: Boolean = false): LowRankGaussianProcess[D] = {
    val trainingDataWithNoise = trainingData.map { case (x, y) => (x, y, sigma2) }

    gp match {
      case gp: SpecializedLowRankGaussianProcess[D] => regressionSpecializedLowRankGP(gp, trainingDataWithNoise, meanOnly)
      case gp => regressionLowRankGP(gp, trainingDataWithNoise, meanOnly)
    }

  }

  def regression[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], trainingData : IndexedSeq[(Point[D], Vector[D], Double)], meanOnly: Boolean = false): LowRankGaussianProcess[D] = {
    gp match {
      case gp: SpecializedLowRankGaussianProcess[D] => regressionSpecializedLowRankGP(gp, trainingData, meanOnly)
      case gp => regressionLowRankGP(gp, trainingData, meanOnly)
    }

  }

  private def regressionLowRankGP[D <: Dim: DimTraits](gp: LowRankGaussianProcess[D], trainingData: IndexedSeq[(Point[D], Vector[D], Double)], meanOnly: Boolean = false): LowRankGaussianProcess[D] = {
    val (lambdas, phis) = gp.eigenPairs.unzip
    val dimTraits = implicitly[DimTraits[D]]
    val outputDim = dimTraits.dimensionality

    val (minv, qtL, yVec, mVec) = GaussianProcess.genericRegressionComputations(gp, trainingData)
    val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)

    val mean_p = gp.instance(mean_coeffs)

    if (meanOnly == true) {
      val emptyEigenPairs = IndexedSeq[(Float, Point[D] => Vector[D])]()
      new LowRankGaussianProcess[D](gp.domain, mean_p, emptyEigenPairs)

    } else {
      val D = breeze.linalg.diag(DenseVector(lambdas.map(math.sqrt(_)).toArray))
      val Sigma = D * minv * D
      val (innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
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
          phisAtXCache = (phisAtXCache + (x, newPhisAtX))._2 // ignore evicted key
          newPhisAtX
        }
        val vec = phisAtX * innerU(::, i)
        dimTraits.createVector(vec.data)
      }

      val phis_p = for (i <- 0 until phis.size) yield ((x : Point[D]) => phip(i)(x))
      val lambdas_p = innerD2.toArray.map(_.toFloat).toIndexedSeq
      new LowRankGaussianProcess[D](gp.domain, mean_p, lambdas_p.zip(phis_p))
    }
  }


  protected[statisticalmodel] def genericRegressionComputations[D <: Dim : DimTraits](gp : LowRankGaussianProcess[D], trainingData: IndexedSeq[(Point[D], Vector[D], Double)])
    : (DenseMatrix[Double], DenseMatrix[Double], DenseVector[Float], DenseVector[Float]) =
    {

      val dimTraits = implicitly[DimTraits[D]]
      val dim = dimTraits.dimensionality
      def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

      val (xs, ys, sigma2s) = trainingData.unzip3

      val yVec = flatten(ys)
      val meanValues = xs.map(gp.mean)
      val mVec = flatten(meanValues)

      val d = gp.outputDimensionality
      val (lambdas, phis) = gp.eigenPairs.unzip

      val Q = DenseMatrix.zeros[Double](trainingData.size * d, phis.size)
      for ((x_i, i) <- xs.zipWithIndex; (phi_j, j) <- phis.zipWithIndex) {
        Q(i * d until i * d + d, j) := phi_j(x_i).toBreezeVector.map(_.toDouble) * math.sqrt(lambdas(j))
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
      (Minv, QtL, yVec, mVec)
    }


  /**
   * Gausssian process regression for a specialzed GP.
   * This implementation explicitly returns a SpecializedLowRankGaussainProcess
   * TODO the implementation is almost the same as for the standard regression. Maybe they couuld be merged
   */
  private def regressionSpecializedLowRankGP[D <: Dim: DimTraits](gp: SpecializedLowRankGaussianProcess[D], trainingData: IndexedSeq[(Point[D], Vector[D], Double)], meanOnly: Boolean = false): SpecializedLowRankGaussianProcess[D] = {

    val dimTraits = implicitly[DimTraits[D]]
    val dim = dimTraits.dimensionality
    val (xs, ys, sigma2s) = trainingData.unzip3
    //def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

    val (lambdas, phis) = gp.eigenPairs.unzip


    val (minv, qtL, yVec, mVec) = GaussianProcess.genericRegressionComputations(gp, trainingData)

    val mean_coeffs = (minv * qtL).map(_.toFloat) * (yVec - mVec)

    gp.instanceAtPoints(mean_coeffs)

    val mean_p = gp.instance(mean_coeffs)
    val mean_pVector = gp.instanceVector(mean_coeffs)

    if (meanOnly == true) {
      // create an empty gaussian process (not specialized), which is needed in order to be able to construct 
      // the specialized one
      val emptyEigenPairs = IndexedSeq[(Float, Point[D] => Vector[D])]()
      val meanOnlyGp = new LowRankGaussianProcess[D](gp.domain, mean_p, emptyEigenPairs)

      new SpecializedLowRankGaussianProcess[D](meanOnlyGp,
        gp.points,
        mean_pVector,
        IndexedSeq[Float](),
        DenseMatrix.zeros[Float](mean_pVector.size, 0))
    } else {
      val D = breeze.linalg.diag(DenseVector(lambdas.map(math.sqrt(_)).toArray))
      val Sigma = D * minv * D
      val (innerUDbl, innerD2, _) = breeze.linalg.svd(Sigma)
      val innerU = innerUDbl.map(_.toFloat)
      @volatile
      var phisAtXCache = ImmutableLRU[Point[D], DenseMatrix[Float]](1000)

      def phip(i: Int)(x: Point[D]): Vector[D] = { // should be phi_p but _ is treated as partial function
        val (maybePhisAtX, newPhisAtXCache) = phisAtXCache.get(x)
        val phisAtX = maybePhisAtX.getOrElse {
          val newPhisAtX = {
            val innerPhisAtx = DenseMatrix.zeros[Float](dim, gp.rank)
            var j = 0;
            while (j < phis.size) {
              val phi_j = phis(j)
              innerPhisAtx(0 until dim, j) := phi_j(x).toBreezeVector
              j += 1
            }
            innerPhisAtx
          }
          phisAtXCache = (phisAtXCache + (x, newPhisAtX))._2 // ignore evicted key
          newPhisAtX
        }
        val vec = phisAtX * innerU(::, i)
        dimTraits.createVector(vec.data)
      }

      val phis_p = for (i <- 0 until phis.size) yield ((x : Point[D])=> phip(i)(x))
      val lambdas_p = innerD2.toArray.map(_.toFloat).toIndexedSeq
      val unspecializedGP = new LowRankGaussianProcess[D](gp.domain, mean_p, lambdas_p.zip(phis_p))

      // we do the follwoing computation
      // val eigenMatrix_p = gp.eigenMatrix * innerU // IS this correct?
      // but in parallel
      val eigenMatrix_p = DenseMatrix.zeros[Float](gp.eigenMatrix.rows, innerU.cols)
      for (rowInd <- (0 until gp.eigenMatrix.rows).par) {
        eigenMatrix_p(rowInd, ::) := gp.eigenMatrix(rowInd, ::) * innerU
      }

      new SpecializedLowRankGaussianProcess(unspecializedGP, gp.points, mean_pVector, lambdas_p, eigenMatrix_p)
    }
  }

}
