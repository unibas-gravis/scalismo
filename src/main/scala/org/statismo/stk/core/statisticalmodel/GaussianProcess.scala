package org.statismo.stk.core
package statisticalmodel

import breeze.linalg.svd.SVD
import breeze.linalg.{*, DenseVector, DenseMatrix}
import org.statismo.stk.core.kernels._
import org.statismo.stk.core.common.ImmutableLRU
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.Domain
import org.statismo.stk.core.geometry.{Point, Vector, Dim}
import org.statismo.stk.core.registration.RigidTransformation


class GaussianProcess[D <: Dim : NDSpace, DO <: Dim : NDSpace] protected (val domain : Domain[D],
                                                                          val mean : Point[D] => Vector[DO],
                                                                          val cov : MatrixValuedPDKernel[D, DO]) {

  protected[this] val dimOps : NDSpace[DO] = implicitly[NDSpace[DO]]

  def outputDimensionality = dimOps.dimensionality

  def sampleAtPoints(pts : Seq[Point[D]]) : Seq[(Point[D], Vector[DO])] = {
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
      .map(data => Vector[DO](data.map(_.toFloat)))
      .toSeq
    pts zip vecs
  }

  /**
   * Compute the marginal distribution for the given point
   */
  def marginal(pt: Point[D]): NDimensionalNormalDistribution[DO] = NDimensionalNormalDistribution(mean(pt), cov(pt, pt))
}




object GaussianProcess {

  def apply[D <: Dim : NDSpace, DO <: Dim : NDSpace](domain : Domain[D],  mean : Point[D] => Vector[DO], cov : MatrixValuedPDKernel[D, DO]) = {
    new GaussianProcess[D, DO](domain, mean, cov)
  }







}
