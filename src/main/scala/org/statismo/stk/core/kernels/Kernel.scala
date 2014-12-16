package org.statismo.stk.core
package kernels

import breeze.linalg.{ DenseVector, pinv, diag, DenseMatrix }
import org.statismo.stk.core.numerics.RandomSVD
import org.statismo.stk.core.numerics.Sampler
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

abstract class MatrixValuedPDKernel[D <: Dim: DimOps, DO <: Dim: DimOps] { self =>

  def apply(x: Point[D], y: Point[D]): MatrixNxN[DO]
  def outputDim = implicitly[DimOps[DO]].toInt

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

case class UncorrelatedKernel1x1(k: PDKernel[_1D]) extends MatrixValuedPDKernel[_1D, _1D] {
  val I = MatrixNxN.eye[_1D]
  def apply(x: Point[_1D], y: Point[_1D]) = I * (k(x, y)) // k is scalar valued
}

case class UncorrelatedKernel2x2(k: PDKernel[_2D]) extends MatrixValuedPDKernel[_2D, _2D] {
  val I = MatrixNxN.eye[_2D]
  def apply(x: Point[_2D], y: Point[_2D]) = I * (k(x, y)) // k is scalar valued
}

case class UncorrelatedKernel3x3(k: PDKernel[_3D]) extends MatrixValuedPDKernel[_3D, _3D] {
  val I = MatrixNxN.eye[_3D]
  def apply(x: Point[_3D], y: Point[_3D]) = I * (k(x, y)) // k is scalar valued
}

// TODO maybe this should be called posterior or conditional kernel
// TODO maybe it should not even be here, but be an internal in the Gaussian process ? Think about
case class LandmarkKernel[D <: Dim: DimOps](k: MatrixValuedPDKernel[D, D], trainingData: IndexedSeq[(Point[D], Vector[D], Double)], memSize: Int) extends MatrixValuedPDKernel[D, D] {

  val dim = implicitly[DimOps[D]].toInt
  val N = trainingData.size * dim
  def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

  val (xs, ys, sigma2s) = trainingData.unzip3

  val noise: DenseMatrix[Double] = breeze.linalg.diag(DenseVector(sigma2s.map(sigma => List.fill(dim)(sigma)).flatten.toArray))

  val K_inv: DenseMatrix[Double] = breeze.linalg.pinv(Kernel.computeKernelMatrix[D](xs, k).map(_.toDouble) + noise)

  def xstar(x: Point[D]) = { Kernel.computeKernelVectorFor[D](x, xs, k) }

  def cov(x: Point[D], y: Point[D]) = {
    k(x, y) - MatrixNxN[D](((xstar(x) * K_inv) * xstar(y)).data.map(_.toFloat))

  }

  val memcov = Memoize.memfun2(cov _, memSize)

  def apply(x: Point[D], y: Point[D]) = {
    memcov(x, y)
  }

}

// TODO this duplicate should not be there
case class LandmarkKernelNonRepeatingPoints[D <: Dim: DimOps](k: MatrixValuedPDKernel[D, D], trainingData: IndexedSeq[(Point[D], Vector[D], Double)], memSize: Int) extends MatrixValuedPDKernel[D, D] {

  val dim = implicitly[DimOps[D]].toInt
  val N = trainingData.size * dim
  def flatten(v: IndexedSeq[Vector[D]]) = DenseVector(v.flatten(_.data).toArray)

  val (xs, ys, sigma2s) = trainingData.unzip3

  val noise = breeze.linalg.diag(DenseVector(sigma2s.map(x => List.fill(dim)(x)).flatten.toArray))

  val K_inv = breeze.linalg.pinv(Kernel.computeKernelMatrix[D](xs, k).map(_.toDouble) + noise)

  def xstar(x: Point[D]) = { Kernel.computeKernelVectorFor[D](x, xs, k) }

  val memxstar = Memoize(xstar, memSize)

  def cov(x: Point[D], y: Point[D]) = {
    k(x, y) - MatrixNxN[D](((memxstar(x) * K_inv) * memxstar(y).t).data.map(_.toFloat))

  }

  def apply(x: Point[D], y: Point[D]) = {
    cov(x, y)
  }

}

case class GaussianKernel3D(val sigma: Double) extends PDKernel[_3D] {
  val sigma2 = sigma * sigma
  def apply(x: Point[_3D], y: Point[_3D]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

case class GaussianKernel2D(val sigma: Double) extends PDKernel[_2D] {
  val sigma2 = sigma * sigma
  def apply(x: Point[_2D], y: Point[_2D]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

case class GaussianKernel1D(val sigma: Double) extends PDKernel[_1D] {

  val sigma2 = sigma * sigma

  def apply(x: Point[_1D], y: Point[_1D]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

case class SampleCovarianceKernel3D(val ts: IndexedSeq[Transformation[_3D]], cacheSizeHint: Int = 100000) extends MatrixValuedPDKernel[_3D, _3D] {

  val ts_memoized = for (t <- ts) yield Memoize(t, cacheSizeHint)

  def mu(x: Point[_3D]): Vector[_3D] = {
    var meanDisplacement = Vector.zeros[_3D]
    var i = 0;
    while (i < ts.size) {
      val t = ts_memoized(i)
      meanDisplacement = meanDisplacement + (t(x) - x)
      i += 1
    }
    meanDisplacement * (1.0 / ts.size)
  }

  @volatile
  var cache = ImmutableLRU[Point[_3D], Vector[_3D]](cacheSizeHint)

  val mu_memoized = Memoize(mu, cacheSizeHint)

  def apply(x: Point[_3D], y: Point[_3D]): MatrixNxN[_3D] = {
    var ms = MatrixNxN.zeros[_3D]
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

  def computeKernelMatrix[D <: Dim](xs: Seq[Point[D]], k: MatrixValuedPDKernel[D, D]): DenseMatrix[Float] = {
    val d = k.outputDim

    val K = DenseMatrix.zeros[Float](xs.size * d, xs.size * d)
    val xiWithIndex = xs.zipWithIndex.par
    val xjWithIndex = xs.zipWithIndex
    for { p1 <- xiWithIndex;  (xi, i) = p1; p2 <- xjWithIndex.drop(i) } {
      val (xj, j) = p2
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
  def computeKernelVectorFor[D <: Dim](x: Point[D], xs: IndexedSeq[Point[D]], k: MatrixValuedPDKernel[D, D]): DenseMatrix[Double] = {
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

  def computeNystromApproximation[D <: Dim: DimOps](k: MatrixValuedPDKernel[D, D], numBasisFunctions: Int, sampler: Sampler[D]): IndexedSeq[(Float, Point[D] => Vector[D])] = {

    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val (ptsForNystrom, _) = sampler.sample.unzip

    // deppending on the sampler, it may happen that we did not sample all the points we wanted 
    val effectiveNumberOfPointsSampled = ptsForNystrom.size

    val kernelMatrix = computeKernelMatrix(ptsForNystrom, k).map(_.toDouble)
    val (uMat, lambdaMat, _) = RandomSVD.computeSVD(kernelMatrix, numBasisFunctions)


    val lambda = lambdaMat.map(lmbda => (lmbda / effectiveNumberOfPointsSampled.toDouble) )
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size

    val W = uMat(::, 0 until numParams) * math.sqrt(effectiveNumberOfPointsSampled) * pinv(diag(lambdaMat(0 until numParams)))

    def computePhis(x: Point[D]): DenseMatrix[Double] = computeKernelVectorFor(x, ptsForNystrom, k) * W
    val computePhisMemoized = Memoize(computePhis, 1000)

    def phi(i: Int)(x: Point[D]) = {
      val value = computePhisMemoized(x)
      // extract the right entry for the i-th phi function
      Vector[D](value(::, i).toArray.map(_.toFloat))

    }

    val lambdaISeq = lambda(0 until numParams).map(_.toFloat).toArray.toIndexedSeq
    val phis = (0 until numParams).map(i => phi(i)_)
    lambdaISeq.zip(phis)
  }

}
