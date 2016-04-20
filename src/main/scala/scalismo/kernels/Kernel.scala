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

import breeze.linalg.{ DenseMatrix, diag, pinv }
import scalismo.common._
import scalismo.geometry._
import scalismo.numerics.PivotedCholesky.NumberOfEigenfunctions
import scalismo.numerics.{ PivotedCholesky, RandomSVD, Sampler }
import scalismo.statisticalmodel.LowRankGaussianProcess.{ Eigenpair, KLBasis }
import scalismo.utils.Memoize

abstract class PDKernel[D <: Dim] { self =>

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

abstract class MatrixValuedPDKernel[D <: Dim: NDSpace, DO <: Dim: NDSpace] { self =>

  def apply(x: Point[D], y: Point[D]): SquareMatrix[DO] = {
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

  protected def k(x: Point[D], y: Point[D]): SquareMatrix[DO]

  def outputDim = implicitly[NDSpace[DO]].dimensionality

  def domain: Domain[D]

  def +(that: MatrixValuedPDKernel[D, DO]): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) + that.k(x, y)
    override def domain = Domain.intersection(self.domain, that.domain)
  }

  def *(that: MatrixValuedPDKernel[D, DO]): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) :* that.k(x, y)
    override def domain = Domain.intersection(self.domain, that.domain)
  }

  def *(s: Double): MatrixValuedPDKernel[D, DO] = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(x, y) * s
    override def domain = self.domain
  }

  // TODO this could be made more generic by allowing the input of phi to be any type A
  def compose(phi: Point[D] => Point[D]) = new MatrixValuedPDKernel[D, DO] {
    override def k(x: Point[D], y: Point[D]) = self.k(phi(x), phi(y))
    override def domain = self.domain
  }

  /**
   * discretize the kernel at the given points
   */
  def discretize(domain: DiscreteDomain[D]): DiscreteMatrixValuedPDKernel[D, DO] = {

    def k(i: PointId, j: PointId): SquareMatrix[DO] = {
      self.k(domain.point(i), domain.point(j))
    }
    DiscreteMatrixValuedPDKernel[D, DO](domain, k)
  }

}
@deprecated("use DiagonalKernel instead", "0.10.0")
case class UncorrelatedKernel[D <: Dim: NDSpace](kernel: PDKernel[D]) extends MatrixValuedPDKernel[D, D] {
  val I = SquareMatrix.eye[D]
  def k(x: Point[D], y: Point[D]) = I * (kernel(x, y)) // k is scalar valued
  override def domain = kernel.domain
}

trait DiagonalKernel[D <: Dim] extends MatrixValuedPDKernel[D, D]

private[kernels] case class IsotropicDiagonalKernel[D <: Dim: NDSpace](kernel: PDKernel[D]) extends DiagonalKernel[D] {
  val I = SquareMatrix.eye[D]
  def k(x: Point[D], y: Point[D]) = I * (kernel(x, y)) // k is scalar valued
  override def domain = kernel.domain
}

private[kernels] case class AnisotropicDiagonalKernel[D <: Dim: NDSpace](kernels: IndexedSeq[PDKernel[D]]) extends DiagonalKernel[D] {
  def k(x: Point[D], y: Point[D]) = SquareMatrix.diag(Vector(kernels.map(k => k(x, y).toFloat).toArray))
  override def domain = kernels.map(_.domain).reduce(Domain.intersection(_, _))
}

object DiagonalKernel {
  def apply[D <: Dim: NDSpace](kernel: PDKernel[D]): DiagonalKernel[D] = IsotropicDiagonalKernel(kernel)
  def apply(xKernel: PDKernel[_2D], yKernel: PDKernel[_2D]): DiagonalKernel[_2D] = AnisotropicDiagonalKernel(IndexedSeq(xKernel, yKernel))
  def apply(xKernel: PDKernel[_3D], yKernel: PDKernel[_3D], zKernel: PDKernel[_3D]): DiagonalKernel[_3D] = AnisotropicDiagonalKernel(IndexedSeq(xKernel, yKernel, zKernel))
}

case class MultiScaleKernel[D <: Dim: NDSpace](kernel: MatrixValuedPDKernel[D, D],
    min: Int,
    max: Int,
    scale: Int => Double = i => scala.math.pow(2.0, -2.0 * i)) extends MatrixValuedPDKernel[D, D] {

  def k(x: Point[D], y: Point[D]): SquareMatrix[D] = {
    var sum = SquareMatrix.zeros[D]
    for (i <- min until max) {
      sum += kernel((x.toVector * Math.pow(2, i)).toPoint, (y.toVector * Math.pow(2, i)).toPoint) * scale(i)
    }
    sum
  }

  // TODO check that the domain is correct
  override def domain = kernel.domain
}

object Kernel {

  def computeKernelMatrix[D <: Dim, DO <: Dim](xs: Seq[Point[D]], k: MatrixValuedPDKernel[D, DO]): DenseMatrix[Float] = {

    val d = k.outputDim

    val K = DenseMatrix.zeros[Float](xs.size * d, xs.size * d)

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
  def computeKernelVectorFor[D <: Dim, DO <: Dim](x: Point[D], xs: IndexedSeq[Point[D]], k: MatrixValuedPDKernel[D, DO]): DenseMatrix[Double] = {
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

  def computeNystromApproximation[D <: Dim: NDSpace, DO <: Dim: NDSpace](k: MatrixValuedPDKernel[D, DO],
    numBasisFunctions: Int,
    sampler: Sampler[D]): KLBasis[D, DO] = {

    // procedure for the nystrom approximation as described in 
    // Gaussian Processes for machine Learning (Rasmussen and Williamson), Chapter 4, Page 99

    val (ptsForNystrom, _) = sampler.sample.unzip

    // depending on the sampler, it may happen that we did not sample all the points we wanted
    val effectiveNumberOfPointsSampled = ptsForNystrom.size

    val (uMat, lambdaMat) = PivotedCholesky.computeApproximateEig(k, ptsForNystrom, 1.0, NumberOfEigenfunctions(numBasisFunctions))

    val lambda = lambdaMat.map(lmbda => (lmbda / effectiveNumberOfPointsSampled.toDouble))
    val numParams = (for (i <- (0 until lambda.size) if lambda(i) >= 1e-8) yield 1).size

    val W = uMat(::, 0 until numParams) * math.sqrt(effectiveNumberOfPointsSampled) * pinv(diag(lambdaMat(0 until numParams)))

    def computePhis(x: Point[D]): DenseMatrix[Double] = computeKernelVectorFor(x, ptsForNystrom, k) * W
    val computePhisMemoized = Memoize(computePhis, 1000)

    def phi(i: Int)(x: Point[D]) = {
      val value = computePhisMemoized(x)
      // extract the right entry for the i-th phi function
      Vector[DO](value(::, i).toArray.map(_.toFloat))

    }

    for (i <- 0 until numParams) yield {
      Eigenpair(lambda(i).toFloat, VectorField(k.domain, phi(i)_))
    }
  }

}
