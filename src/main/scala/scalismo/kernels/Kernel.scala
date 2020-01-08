/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.kernels

import breeze.linalg.{diag, pinv, DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.geometry._
import scalismo.numerics.PivotedCholesky.RelativeTolerance
import scalismo.numerics.{PivotedCholesky, Sampler}
import scalismo.statisticalmodel.LowRankGaussianProcess.{Eigenpair, KLBasis}
import scalismo.utils.Memoize

abstract class PDKernel[D] {
  self =>

  protected def k(x: Point[D], y: Point[D]): Double

  def apply(x: Point[D], y: Point[D]): Double = {
    if (this.domain.isDefinedAt(x) && this.domain.isDefinedAt(y))
      k(x, y)
    else {
      if (!this.domain.isDefinedAt(x)) {
        throw new IllegalArgumentException((s"$x is outside of the domain"))
      } else {
        throw new IllegalArgumentException((s"$y is outside of the domain"))
      }
    }
  }

  def domain: Domain[D]

  def +(that: PDKernel[D]): PDKernel[D] = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) + that.k(x, y)

    override def domain = Domain.intersection(self.domain, that.domain)
  }

  def *(that: PDKernel[D]): PDKernel[D] = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * that.k(x, y)

    override def domain = Domain.intersection(self.domain, that.domain)
  }

  def *(s: Double): PDKernel[D] = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * s

    override def domain = self.domain
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new PDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(phi(x), phi(y))

    override def domain = self.domain
  }

}

abstract class MatrixValuedPDKernel[D: NDSpace] {
  self =>

  def apply(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
    if (this.domain.isDefinedAt(x) && this.domain.isDefinedAt(y))
      k(x, y)
    else {
      if (!this.domain.isDefinedAt(x)) {
        throw new IllegalArgumentException((s"$x is outside of the domain"))
      } else {
        throw new IllegalArgumentException((s"$y is outside of the domain"))
      }
    }
  }

  protected def k(x: Point[D], y: Point[D]): DenseMatrix[Double]

  def outputDim: Int

  def domain: Domain[D]

  def +(that: MatrixValuedPDKernel[D]): MatrixValuedPDKernel[D] = {
    assert(this.outputDim == that.outputDim)
    new MatrixValuedPDKernel[D] {
      override def k(x: Point[D], y: Point[D]) = self.k(x, y) + that.k(x, y)

      override def domain = Domain.intersection(self.domain, that.domain)

      override def outputDim = self.outputDim
    }
  }

  def *(that: MatrixValuedPDKernel[D]): MatrixValuedPDKernel[D] = {
    assert(this.outputDim == that.outputDim)
    new MatrixValuedPDKernel[D] {
      override def k(x: Point[D], y: Point[D]) = self.k(x, y) *:* that.k(x, y)

      override def domain = Domain.intersection(self.domain, that.domain)

      override def outputDim = self.outputDim
    }
  }

  def *(s: Double): MatrixValuedPDKernel[D] = new MatrixValuedPDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * s

    override def domain = self.domain

    override def outputDim = self.outputDim
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new MatrixValuedPDKernel[D] {
    override def k(x: Point[D], y: Point[D]) = self.k(phi(x), phi(y))

    override def domain = self.domain

    override def outputDim = self.outputDim
  }

  /**
   * discretize the kernel at the given points
   */
  def discretize(domain: DiscreteDomain[D]): DiscreteMatrixValuedPDKernel[D] = {

    def k(i: PointId, j: PointId): DenseMatrix[Double] = {
      self.k(domain.point(i), domain.point(j))
    }

    DiscreteMatrixValuedPDKernel[D](domain, k, outputDim)
  }

}

trait DiagonalKernel[D] extends MatrixValuedPDKernel[D]

private[kernels] case class IsotropicDiagonalKernel[D: NDSpace](kernel: PDKernel[D], override val outputDim: Int)
    extends DiagonalKernel[D] {
  val I = DenseMatrix.eye[Double](outputDim)

  def k(x: Point[D], y: Point[D]) = I * kernel(x, y)

  // k is scalar valued
  override def domain = kernel.domain
}

private[kernels] case class AnisotropicDiagonalKernel[D: NDSpace](kernels: IndexedSeq[PDKernel[D]])
    extends DiagonalKernel[D] {
  def k(x: Point[D], y: Point[D]) = diag(DenseVector[Double](kernels.map(k => k(x, y)).toArray))

  override def domain = kernels.map(_.domain).reduce(Domain.intersection(_, _))

  override def outputDim = kernels.length
}

object DiagonalKernel {
  def apply[D: NDSpace](kernel: PDKernel[D], outputDim: Int): DiagonalKernel[D] =
    IsotropicDiagonalKernel(kernel, outputDim)

  def apply(xKernel: PDKernel[_2D], yKernel: PDKernel[_2D]): DiagonalKernel[_2D] =
    AnisotropicDiagonalKernel(IndexedSeq(xKernel, yKernel))

  def apply(xKernel: PDKernel[_3D], yKernel: PDKernel[_3D], zKernel: PDKernel[_3D]): DiagonalKernel[_3D] =
    AnisotropicDiagonalKernel(IndexedSeq(xKernel, yKernel, zKernel))
}

case class MultiScaleKernel[D: NDSpace](kernel: MatrixValuedPDKernel[D],
                                        min: Int,
                                        max: Int,
                                        scale: Int => Double = i => scala.math.pow(2.0, -2.0 * i))
    extends MatrixValuedPDKernel[D] {

  override def outputDim = kernel.outputDim

  def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
    val sum = DenseMatrix.zeros[Double](outputDim, outputDim)
    for (i <- min until max) {
      sum += kernel((x.toVector * Math.pow(2, i)).toPoint, (y.toVector * Math.pow(2, i)).toPoint) * scale(i)
    }
    sum
  }

  // TODO check that the domain is correct
  override def domain = kernel.domain
}

object Kernel {

  def computeKernelMatrix[D](xs: Seq[Point[D]], k: MatrixValuedPDKernel[D]): DenseMatrix[Double] = {

    val d = k.outputDim

    val K = DenseMatrix.zeros[Double](xs.size * d, xs.size * d)

    var i = 0
    while (i < xs.size) {
      var j = i
      while (j < xs.size) {

        val kxixj = k(xs(i), xs(j))
        var di = 0
        while (di < d) {
          var dj = 0
          while (dj < d) {
            K(i * d + di, j * d + dj) = kxixj(di, dj)
            K(j * d + dj, i * d + di) = kxixj(di, dj)
            dj += 1
          }
          di += 1
        }
        j += 1
      }
      i += 1
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
  def computeKernelVectorFor[D](x: Point[D],
                                xs: IndexedSeq[Point[D]],
                                k: MatrixValuedPDKernel[D]): DenseMatrix[Double] = {
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

  /**
   * Computes the leading eigenvalues / eigenfunctions of the integral operator corresponding
   * to kernel k. The number of leading eigenfunctions is at most n, where n is the number of points sampled.
   * If the eigenvalues are decaying quickly, it can be much smaller than n.
   *
   * @param k (matrix-valued) kernel
   * @param sampler  A point sampler, which determines the points that are used to compute the approximation.
   * @return The leading eigenvalue / eigenfunction pairs
   */
  def computeNystromApproximation[D: NDSpace, Value](k: MatrixValuedPDKernel[D], sampler: Sampler[D])(
    implicit vectorizer: Vectorizer[Value]
  ): KLBasis[D, Value] = {

    // procedure for the nystrom approximation as described in
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val (ptsForNystrom, _) = sampler.sample().unzip
    // depending on the sampler, it may happen that we did not sample all the points we wanted
    val effectiveNumberOfPointsSampled = ptsForNystrom.size

    // we compute the eigenvectors only approximately, to a tolerance of 1e-5. As the nystrom approximation is
    // anyway not exact, this should be sufficient for all practical cases.
    val (uMat, lambdaMat) = PivotedCholesky.computeApproximateEig(k, ptsForNystrom, RelativeTolerance(1e-5))

    val lambda = lambdaMat.map(lmbda => (lmbda / effectiveNumberOfPointsSampled.toDouble))
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size

    val W = uMat(::, 0 until numParams) * math.sqrt(effectiveNumberOfPointsSampled) * pinv(
      diag(lambdaMat(0 until numParams))
    )

    def computePhis(x: Point[D]): DenseMatrix[Double] = computeKernelVectorFor(x, ptsForNystrom, k) * W
    val computePhisMemoized = Memoize(computePhis, 1000)

    def phi(i: Int)(x: Point[D]) = {
      val value = computePhisMemoized(x)
      // extract the right entry for the i-th phi function
      vectorizer.unvectorize(value(::, i).toDenseVector)
    }

    for (i <- 0 until numParams) yield {
      Eigenpair(lambda(i), Field(k.domain, phi(i) _))
    }
  }

}
