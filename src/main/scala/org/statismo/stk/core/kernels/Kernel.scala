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

abstract class MatrixValuedPDKernel[D <: Dim: NDSpace, DO <: Dim: NDSpace] { self =>

  def apply(x: Point[D], y: Point[D]): SquareMatrix[DO]
  def outputDim = implicitly[NDSpace[DO]].dimensionality

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

case class UncorrelatedKernel[D <: Dim : NDSpace](k: PDKernel[D]) extends MatrixValuedPDKernel[D, D] {
  val I = SquareMatrix.eye[D]
  def apply(x: Point[D], y: Point[D]) = I * (k(x, y)) // k is scalar valued
}



case class MultiScaleKernel[D <: Dim : NDSpace](kernel : MatrixValuedPDKernel[D, D],
                                                min: Int,
                                                max: Int,
                                                scale : Int => Double = i => scala.math.pow(2.0, -2.0 * i)) extends MatrixValuedPDKernel[D, D] {

  def apply(x: Point[D], y: Point[D]): SquareMatrix[D] = {
    var sum = SquareMatrix.zeros[D]
    for (i <- min until max) {
      sum += kernel((x.toVector * Math.pow(2, -i)).toPoint, (y.toVector * Math.pow(2, -i)).toPoint) * scale(i)
    }
    sum
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

  def computeNystromApproximation[D <: Dim: NDSpace](k: MatrixValuedPDKernel[D, D], numBasisFunctions: Int, sampler: Sampler[D]): IndexedSeq[(Float, Point[D] => Vector[D])] = {

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
