package org.statismo.stk.core.numerics

import breeze.linalg.{diag, DenseMatrix, DenseVector}
import org.statismo.stk.core.utils
import org.statismo.stk.core.utils.Benchmark

/** 
 * Implementation of a Randomized approach for SVD, 
 * as proposed in 
 * Finding structure with randomness: Probabilistic algorithms for constructing approximate matrix decompositions
 * N Halko, PG Martinsson, JA Tropp - SIAM review, 2011 - SIAM
 */
object RandomSVD {

  def computeSVD(A : DenseMatrix[Double], k : Int, p : Int = 10) : (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])= {
    require(A.rows ==  A.cols) // might be removed later (check in Halko paper)
    val m = A.rows

	  val standardNormal = breeze.stats.distributions.Gaussian(0, 1)
	  
	  // create a gaussian random matrix
	  val Omega = DenseMatrix.zeros[Double](m, k + p).map(_ => standardNormal.draw)
	  val Y = (A.t * (A * (A.t * (A * Omega))))

	  val (qfull, _) = breeze.linalg.qr(Y)

    val q = qfull(::, 0 until k + p)
	  val B = q.t * A

	  val (uHat, sigma, vt) = breeze.linalg.svd(B)
	  val U = q * uHat
	  (U(::, 0 until k), sigma(0 until k), vt(0 until k, 0 until k))
  }

}