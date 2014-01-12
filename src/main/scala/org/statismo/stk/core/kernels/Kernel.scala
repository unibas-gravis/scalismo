package org.statismo.stk.core
package kernels

import breeze.linalg.{ pinv, diag, DenseMatrix }
import org.statismo.stk.core.image.DiscreteImageDomain1D
import breeze.linalg.DenseVector
import org.statismo.stk.core.numerics.RandomSVD
import org.statismo.stk.core.image.DiscreteImageDomain
import breeze.plot.Figure
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.common.DiscreteDomain
import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.common.ImmutableLRU
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.utils.Memoize

abstract class PDKernel[D <: Dim] { self =>
  def apply(x: Point[D], y: Point[D]): Double

  def +(that: PDKernel[D]): PDKernel[D] = new PDKernel[D] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(x, y) + that.apply(x, y)
  }

  def *(that: PDKernel[D]): PDKernel[D] = new PDKernel[D] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(x, y) * that.apply(x, y)
  }

  def *(s: Double): PDKernel[D] = new PDKernel[D] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(x, y) * s
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new PDKernel[D] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(phi(x), phi(y))
  }

}

abstract class MatrixValuedPDKernel[D <: Dim, DO <: Dim: DimTraits] { self =>
  val oDimTraits = implicitly[DimTraits[DO]]

  def apply(x: Point[D], y: Point[D]): MatrixNxN[DO]
  def outputDim = oDimTraits.dimensionality

  def +(that: MatrixValuedPDKernel[D, DO]): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(x, y) + that.apply(x, y)
  }

  def *(that: MatrixValuedPDKernel[D, DO]): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(x, y) :* that.apply(x, y)
  }

  def *(s: Double): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(x, y) * s
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new MatrixValuedPDKernel[D, DO] {
    override def apply(x: Point[D], y: Point[D]) = self.apply(phi(x), phi(y))
  }

}

case class UncorrelatedKernel1x1(k: PDKernel[OneD]) extends MatrixValuedPDKernel[OneD, OneD] {
  val I = Matrix1x1.eye
  def apply(x: Point[OneD], y: Point[OneD]) = I * (k(x, y)) // k is scalar valued
}

case class UncorrelatedKernel2x2(k: PDKernel[TwoD]) extends MatrixValuedPDKernel[TwoD, TwoD] {
  val I = Matrix2x2.eye
  def apply(x: Point[TwoD], y: Point[TwoD]) = I * (k(x, y)) // k is scalar valued
}

case class UncorrelatedKernel3x3(k: PDKernel[ThreeD]) extends MatrixValuedPDKernel[ThreeD, ThreeD] {
  val I = Matrix3x3.eye
  def apply(x: Point[ThreeD], y: Point[ThreeD]) = I * (k(x, y)) // k is scalar valued
}

case class GaussianKernel3D(val sigma: Double) extends PDKernel[ThreeD] {
  val sigma2 = sigma * sigma
  def apply(x: Point[ThreeD], y: Point[ThreeD]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

case class GaussianKernel2D(val sigma: Double) extends PDKernel[TwoD] {
  val sigma2 = sigma * sigma
  def apply(x: Point[TwoD], y: Point[TwoD]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

case class GaussianKernel1D(val sigma: Double) extends PDKernel[OneD] {

  val sigma2 = sigma * sigma

  def apply(x: Point[OneD], y: Point[OneD]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

case class SampleCovarianceKernel3D(val ts: IndexedSeq[Transformation[ThreeD]], cacheSizeHint: Int = 100000) extends MatrixValuedPDKernel[ThreeD, ThreeD] {
  val dimTraits3D = implicitly[DimTraits[ThreeD]]

  val ts_memoized = for (t <- ts) yield Memoize(t, cacheSizeHint)

  def mu(x: Point[ThreeD]): Vector[ThreeD] = {
    var meanDisplacement = dimTraits3D.zeroVector
    var i = 0;
    while (i < ts.size) {
      val t = ts_memoized(i)
      meanDisplacement = meanDisplacement + (t(x) - x)
      i += 1
    }
    meanDisplacement * (1.0 / ts.size)
  }

  @volatile
  var cache = ImmutableLRU[Point[ThreeD], Vector[ThreeD]](cacheSizeHint)

  val mu_memoized = Memoize(mu, cacheSizeHint)

  def apply(x: Point[ThreeD], y: Point[ThreeD]): Matrix3x3 = {
    var ms = Matrix3x3.zeros
    var i = 0;
    while (i < ts.size) {
      val t = ts_memoized(i)
      val ux = t(x) - x
      val uy = t(y) - y
      ms = ms + (ux - mu_memoized(x)).outer(uy - mu_memoized(y))
      i += 1
    }
    ms * (1f / (ts.size - 1))
  }

}

object Kernel {


  def computeKernelMatrix[D <: Dim](xs: IndexedSeq[Point[D]], k: MatrixValuedPDKernel[D, D]): DenseMatrix[Float] = {
    val d = k.outputDim

    val K = DenseMatrix.zeros[Float](xs.size * d, xs.size * d)
    val xiWithIndex = xs.zipWithIndex.par
    val xjWithIndex = xs.zipWithIndex
    for { (xi, i) <- xiWithIndex; (xj, j) <- xjWithIndex.drop(i) } {
      val kxixj = k(xi, xj);
      var di = 0;
      while (di < d) {
        var dj = 0;
        while (dj < d) {
          K(i * d + di, j * d + dj) = kxixj(di, dj)
          K(j * d + dj, i * d + di) = K(i * d + di, j * d + dj)
          dj += 1
        }
        di += 1
      }
    }
    K
  }
  
  /**
   * for every domain point x in the list, we compute the kernel vector
   * kx = (k(x, x1), ... k(x, xm))
   * since the kernel is matrix valued, kx is actually a matrix
   *
   * !! Hack - We currently return a double matrix, with the only reason that matrix multiplication (further down) is
   * faster (breeze implementation detail). This should be replaced at some point
   */
  private def computeKernelVectorFor[D <: Dim](x: Point[D], xs: IndexedSeq[Point[D]], k: MatrixValuedPDKernel[D, D]): DenseMatrix[Double] = {
    val d = k.outputDim

    val kxs = DenseMatrix.zeros[Double](d, xs.size * d)

    var j = 0
    while (j < xs.size) {
      var di = 0
      val kxxj = k(x, xs(j))
      while (di < d) {
        var dj = 0
        while (dj < d) {
          kxs(di, j * d + dj) = kxxj(di, dj)
          dj += 1
        }
        di += 1
      }
      j += 1
    }

    kxs
  }

  def computeNystromApproximation[D <: Dim: DimTraits](k: MatrixValuedPDKernel[D, D], numBasisFunctions: Int, sampler: Sampler[D, Point[D]]): IndexedSeq[(Float, Point[D] => Vector[D])] = {

    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val createVector = implicitly[DimTraits[D]].createVector _ // get the create vector function for the right dimension

    val volumeOfSampleRegion = sampler.volumeOfSampleRegion

    val (ptsForNystrom, _) = sampler.sample.unzip

    // deppending on the sampler, it may happen that we did not sample all the points we wanted 
    val effectiveNumberOfPointsSampled = ptsForNystrom.size

    val kernelMatrix = computeKernelMatrix(ptsForNystrom, k).map(_.toDouble)
    val (uMat, lambdaMat, _) = RandomSVD.computeSVD(kernelMatrix, numBasisFunctions)

    // TODO check that this is also correct for non-rectangular domains
    val lambda = lambdaMat.map(lmbda => (volumeOfSampleRegion / effectiveNumberOfPointsSampled.toDouble) * lmbda)
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size

    val W = uMat(::, 0 until numParams) * math.sqrt(effectiveNumberOfPointsSampled / volumeOfSampleRegion) * pinv(diag(lambdaMat(0 until numParams)))

    def computePhis(x: Point[D]): DenseMatrix[Double] = computeKernelVectorFor(x, ptsForNystrom, k) * W
    val computePhisMemoized = Memoize(computePhis, 1000)

    def phi(i: Int)(x: Point[D]) = {
      val value = computePhisMemoized(x)
      // extract the right entry for the i-th phi function
      createVector(value(::, i).toArray.map(_.toFloat))

    }

    val lambdaISeq = lambda(0 until numParams).map(_.toFloat).toArray.toIndexedSeq
    val phis = (0 until numParams).map(i => phi(i)_)
    lambdaISeq.zip(phis)
  }

}
