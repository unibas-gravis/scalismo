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

package scalismo.numerics

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.pow
import scalismo.geometry._
import scalismo.kernels.{MatrixValuedPDKernel, PDKernel}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable.ParVector

/**
 * Result object for the pivoted cholesky of a matrix A
 * @param L
 *   The (first m columns) of a lower triangular matrix L, for which LL' = A_m \approx A.
 * @param p
 *   The pivot
 * @param tr
 *   : The trace of the matrix (A_m - A) (i.e. the approximation error)
 */
case class PivotedCholesky(L: DenseMatrix[Double], p: IndexedSeq[Int], tr: Double)

private class PivotedCholeskyFactor(d: Int, sizeHint: Int = 20) {
  val cols = new ArrayBuffer[DenseVector[Double]](sizeHint)

  def apply(row: Int, col: Int): Double = cols(col)(row)

  def col(i: Int): DenseVector[Double] = cols(i)

  def addCol(vec: DenseVector[Double]): Unit = {
    require(vec.length == d)
    cols += vec
  }

  def setCol(col: Int, vec: DenseVector[Double]): Unit = {
    require(col >= 0 && col < cols.size + 1)
    require(vec.length == d)
    if (col < cols.size)
      cols(col) = vec
    else
      cols += vec
  }

  def toDenseMatrix: DenseMatrix[Double] = {
    val m = DenseMatrix.zeros[Double](d, cols.size)
    for (i <- cols.indices) {
      m(::, i) := cols(i)
    }
    m
  }
}

object PivotedCholesky {

  sealed trait StoppingCriterion

  case class AbsoluteTolerance(tol: Double) extends StoppingCriterion

  case class RelativeTolerance(tol: Double) extends StoppingCriterion

  case class NumberOfEigenfunctions(n: Int) extends StoppingCriterion

  private[this] def computeApproximateCholeskyGeneric[A](kernel: (A, A) => Double,
                                                         xs: IndexedSeq[A],
                                                         stoppingCriterion: StoppingCriterion
  ): PivotedCholesky = {

    val n = xs.size
    val p = scala.collection.mutable.ArrayBuffer.range(0, n)
    val d = scala.collection.mutable.ArrayBuffer.tabulate(n)(i => kernel(xs(i), xs(i)))

    def swapP(k: Int, pivl: Int) = {

      val tmp = p(k)
      p(k) = p(pivl)
      p(pivl) = tmp

    }

    var tr: Double = d.sum
    var k = 0

    val ids = (k until n)

    val (tolerance, maxNumEigenfunctions) = stoppingCriterion match {
      case AbsoluteTolerance(t)                 => (t, n)
      case RelativeTolerance(t)                 => (tr * t, n)
      case NumberOfEigenfunctions(numEigenfuns) => (1e-15, numEigenfuns)
    }

    val L = new PivotedCholeskyFactor(n, n)

    while (k < n && k < maxNumEigenfunctions && tr >= tolerance) {

      val S = DenseVector.zeros[Double](n)

      val pivl = (k until n).map(i => (i, d(p(i)))).maxBy(_._2)._1

      swapP(k, pivl)

      val D = Math.sqrt(d(p(k)))
      S(p(k)) = D

      val pointIds = ids.splitAt(k + 1)._2
      val chunks = new ParVector(
        pointIds.grouped(Math.max(1, n / Runtime.getRuntime().availableProcessors())).toVector
      )

      var c = 0

      while (c < k) {
        val tmp = L(p(k), c)
        for (r <- new ParVector(pointIds.toVector)) {
          S(p(r)) += L(p(r), c) * tmp
        }
        c += 1
      }

      def sumChunk(ids: IndexedSeq[Int]): Double = {

        var s = 0.0
        var r = ids.head
        while (r <= ids.last) {

          S(p(r)) = (kernel(xs(p(r)), xs(p(k))) - S(p(r))) / D
          d(p(r)) = d(p(r)) - (S(p(r)) * S(p(r)))
          s += d(p(r))
          r += 1

        }
        s
      }

      val chunksum = for (pointChunk <- chunks) yield {
        sumChunk(pointChunk)
      }

      d(p(k)) = d(p(k)) - (S(p(k)) * S(p(k)))

      tr = d(p(k)) + chunksum.sum

      L.addCol(DenseVector(S.toArray))
      k += 1
    }

    PivotedCholesky(L.toDenseMatrix, p.toIndexedSeq, tr)
  }

  def computeApproximateCholesky[D: NDSpace, DO: NDSpace](kernel: MatrixValuedPDKernel[D],
                                                          xs: IndexedSeq[Point[D]],
                                                          stoppingCriterion: StoppingCriterion
  ): PivotedCholesky = {

    case class PointWithDim(point: Point[D], dim: Int)
    val dim = NDSpace[DO].dimensionality
    val xsWithDim: IndexedSeq[PointWithDim] = xs.flatMap(f => (0 until dim).map(i => PointWithDim(f, i)))
    def kscalar(x: PointWithDim, y: PointWithDim): Double = kernel(x.point, y.point)(x.dim, y.dim)

    computeApproximateCholeskyGeneric[PointWithDim](kscalar, xsWithDim, stoppingCriterion)

  }

  def computeApproximateCholesky[D: NDSpace](kernel: PDKernel[D],
                                             xs: IndexedSeq[Point[D]],
                                             stoppingCriterion: StoppingCriterion
  ): PivotedCholesky = {
    val k: (Point[D], Point[D]) => Double = (x, y) => kernel(x, y)
    computeApproximateCholeskyGeneric[Point[D]](k, xs, stoppingCriterion)
  }

  def computeApproximateCholesky(A: DenseMatrix[Double], stoppingCriterion: StoppingCriterion): PivotedCholesky = {

    require(A.cols == A.rows)
    val kernel: (Int, Int) => Double = (i, j) => A(i, j)
    val indices = IndexedSeq.range(0, A.cols)
    computeApproximateCholeskyGeneric[Int](kernel, indices, stoppingCriterion)
  }

  private def computeApproximateEigGeneric[A](k: (A, A) => Double, xs: IndexedSeq[A], sc: StoppingCriterion) = {

    val PivotedCholesky(l, _, _) = computeApproximateCholeskyGeneric(k, xs, sc)

    val Lt = l(::, 0 until l.cols).t
    val phi: DenseMatrix[Double] = Lt * l(::, 0 until l.cols)

    val SVD(v, _, _) = breeze.linalg.svd(phi)
    val U: DenseMatrix[Double] = l(::, 0 until l.cols) * v

    U
  }

  private def extractEigenvalues(U: DenseMatrix[Double]) = {

    val d: DenseVector[Double] = DenseVector.zeros(U.cols)

    for (i <- 0 until U.cols) {
      d(i) = breeze.linalg.norm(U(::, i))
      U(::, i) := U(::, i) * 1.0 / d(i)
    }

    (U, pow(d, 2))

  }

  def computeApproximateEig(A: DenseMatrix[Double], sc: StoppingCriterion) = {
    val kernel: (Int, Int) => Double = (i, j) => A(i, j)
    val indices = IndexedSeq.range(0, A.cols)

    extractEigenvalues(computeApproximateEigGeneric(kernel, indices, sc))
  }

  def computeApproximateEig[D: NDSpace](kernel: MatrixValuedPDKernel[D],
                                        xs: IndexedSeq[Point[D]],
                                        stoppingCriterion: StoppingCriterion
  ) = {

    case class PointWithDim(point: Point[D], dim: Int)
    val dim = kernel.outputDim
    val xsWithDim: IndexedSeq[PointWithDim] = xs.flatMap(f => (0 until dim).map(i => PointWithDim(f, i)))
    def kscalar(x: PointWithDim, y: PointWithDim): Double = kernel(x.point, y.point)(x.dim, y.dim)

    extractEigenvalues(computeApproximateEigGeneric[PointWithDim](kscalar, xsWithDim, stoppingCriterion))

  }

  def computeApproximateEig[D: NDSpace, DO: NDSpace](kernel: PDKernel[D],
                                                     xs: IndexedSeq[Point[D]],
                                                     D: Double,
                                                     stoppingCriterion: StoppingCriterion
  ) = {

    def k(x: Point[D], y: Point[D]): Double = kernel(x, y)

    extractEigenvalues(computeApproximateEigGeneric[Point[D]](k, xs, stoppingCriterion))

  }
}
