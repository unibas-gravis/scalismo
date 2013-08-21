package org.statismo.stk.core.numerics

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

/** 
 * Implementation of a Randomized approach for SVD, 
 * as proposed in 
 * TODO add citation
 */
object RandomSVD {

  def computeSVD(A : DenseMatrix[Double], k : Int) : (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])= {
    val (m, n )= (A.rows, A.cols)
	  val Omega = DenseMatrix.zeros[Double](n, k)
	  val standardNormal = breeze.stats.distributions.Gaussian(0, 1)
	  
	  // create a gaussian random matrix
	  Omega.map(_ => standardNormal.draw)
	  val Y = (A * (A.t * (A * Omega)))
	  val (q, r) = breeze.linalg.qr(A)
	  val B = q.t * A
	  val (uHat, sigma, vt) = breeze.linalg.svd(B)
	  val U = q * uHat
	  (U(::, 0 until k), sigma(0 until k), vt(0 until k, 0 until k))
  }
}