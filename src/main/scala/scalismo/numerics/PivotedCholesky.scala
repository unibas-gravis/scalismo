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
import breeze.linalg.{ DenseMatrix, DenseVector }
import breeze.numerics.pow
import scalismo.geometry._
import scalismo.kernels.{ MatrixValuedPDKernel, PDKernel }
import scalismo.utils.Benchmark

/**
 * Result object for the pivoted cholesky of a matrix A
 *
 * @param L  The (first m columns) of a lower triangular matrix L, for which LL' = A_m \approx A.
 * @param p  The pivot
 * @param tr : The trace of the matrix (A_m - A) (i.e. the approximation error)
 */
case class PivotedCholesky(L: DenseMatrix[Double], p: IndexedSeq[Int], tr: Double)

object PivotedCholesky {

  sealed trait StoppingCriterion

  case class AbsoluteTolerance(tol: Double) extends StoppingCriterion

  case class RelativeTolerance(tol: Double) extends StoppingCriterion

  case class NumberOfEigenfunctions(n: Int) extends StoppingCriterion

  private[this] def computeApproximateCholeskyGeneric[A](kernel: (A, A) => Double,
    xs: IndexedSeq[A],
    stoppingCriterion: StoppingCriterion): PivotedCholesky = {

    val n = xs.size

    val p = scala.collection.mutable.IndexedSeq.range(0, n)

    val diagData = for (x <- xs) yield kernel(x, x)
    val d = DenseVector[Double](diagData.toArray)

    var tr: Double = breeze.linalg.sum(d)
    var k = 0

    val (tolerance, maxNumEigenfunctions) = stoppingCriterion match {
      case AbsoluteTolerance(t) => (t, n)
      case RelativeTolerance(t) => (tr * t, n)
      case NumberOfEigenfunctions(numEigenfuns) => (1e-15, numEigenfuns)
    }

    // The matrix will hold the result (i.e. LL' is the resulting kernel matrix). As we do not know the
    // number of columns we compute until we have the desired accuracy, the matrix is updated in each iteration.
    var L: DenseMatrix[Double] = null

    // we either loop until we have the required number of eigenfunction, the precition or
    // the trace is not decreasing anymore (which is a sign of numerical instabilities)
    while (k < n && k < maxNumEigenfunctions && tr >= tolerance) {

      L = if (k == 0) {
        DenseMatrix.zeros[Double](n, 1)
      } else {
        val Lnew = DenseMatrix.zeros[Double](n, k + 1)
        Lnew(::, 0 until k) := L(::, 0 until k)
        Lnew
      }

      // get biggest element for pivot and switch
      val pivl = /*k + */ (k until n).map(i => (i, d(p(i)))).maxBy(_._2)._1

      val tmp = p(k)
      p(k) = p(pivl)
      p(pivl) = tmp
      //println("dpk: " + d(p(k)))

      L(p(k), k) = Math.sqrt(d(p(k)))
      val Adata = p.slice(k + 1, n).par.map(i => kernel(xs(i), xs(p(k))))
      val AMat = DenseMatrix.create(n - k - 1, 1, Adata.toArray)
      L(p.slice(k + 1, n), IndexedSeq(k)) := AMat / L(p(k), k)

      // update L
      val rhs = (L(p.slice(k + 1, n), IndexedSeq.range(0, k)).toDenseMatrix * L(IndexedSeq(p(k)), IndexedSeq.range(0, k)).toDenseMatrix.t) / L(p(k), k)
      L(p.slice(k + 1, n), IndexedSeq(k)) := (L(p.slice(k + 1, n), IndexedSeq(k)).toDenseMatrix - rhs)

      // update d
      val lll = L(p.slice(k, n), IndexedSeq(k)).flatten().toDenseVector.map(e => Math.pow(e, 2))
      val ddd = d(p.slice(k, n)).toDenseVector
      d(p.slice(k, n)) := ddd - lll
      tr = breeze.linalg.sum(d(p.slice(k, n)).toDenseVector)

      k += 1
    }

    PivotedCholesky(L, p, tr)

  }

  def computeApproximateCholesky[D <: Dim: NDSpace, DO <: Dim: NDSpace](kernel: MatrixValuedPDKernel[D],
    xs: IndexedSeq[Point[D]],
    stoppingCriterion: StoppingCriterion): PivotedCholesky = {

    case class PointWithDim(point: Point[D], dim: Int)
    val dim = NDSpace[DO].dimensionality
    val xsWithDim: IndexedSeq[PointWithDim] = xs.flatMap(f => (0 until dim).map(i => PointWithDim(f, i)))
    def kscalar(x: PointWithDim, y: PointWithDim): Double = kernel(x.point, y.point)(x.dim, y.dim)

    computeApproximateCholeskyGeneric[PointWithDim](kscalar, xsWithDim, stoppingCriterion)

  }

  def computeApproximateCholesky[D <: Dim: NDSpace](kernel: PDKernel[D],
    xs: IndexedSeq[Point[D]],
    stoppingCriterion: StoppingCriterion): PivotedCholesky = {
    val k: (Point[D], Point[D]) => Double = (x, y) => kernel(x, y)
    computeApproximateCholeskyGeneric[Point[D]](k, xs, stoppingCriterion)
  }

  def computeApproximateCholesky(A: DenseMatrix[Double], stoppingCriterion: StoppingCriterion): PivotedCholesky = {

    require(A.cols == A.rows)
    val kernel: (Int, Int) => Double = (i, j) => A(i, j)
    val indices = IndexedSeq.range(0, A.cols)
    computeApproximateCholeskyGeneric[Int](kernel, indices, stoppingCriterion)
  }

  private def computeApproximateEigGeneric[A](k: (A, A) => Double, xs: IndexedSeq[A], D: Double, sc: StoppingCriterion) = {

    val PivotedCholesky(l, _, _) = computeApproximateCholeskyGeneric(k, xs, sc)

    val LD = l(::, 0 until l.cols).t :* D
    val phi: DenseMatrix[Double] = LD * l(::, 0 until l.cols)

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

  def computeApproximateEig(A: DenseMatrix[Double], D: Double, sc: StoppingCriterion) = {
    val kernel: (Int, Int) => Double = (i, j) => A(i, j)
    val indices = IndexedSeq.range(0, A.cols)

    extractEigenvalues(computeApproximateEigGeneric(kernel, indices, D, sc))
  }

  def computeApproximateEig[D <: Dim: NDSpace](kernel: MatrixValuedPDKernel[D],
    xs: IndexedSeq[Point[D]], D: Double,
    stoppingCriterion: StoppingCriterion) = {

    case class PointWithDim(point: Point[D], dim: Int)
    val dim = kernel.outputDim
    val xsWithDim: IndexedSeq[PointWithDim] = xs.flatMap(f => (0 until dim).map(i => PointWithDim(f, i)))
    def kscalar(x: PointWithDim, y: PointWithDim): Double = kernel(x.point, y.point)(x.dim, y.dim)

    extractEigenvalues(computeApproximateEigGeneric[PointWithDim](kscalar, xsWithDim, D, stoppingCriterion))

  }

  def computeApproximateEig[D <: Dim: NDSpace, DO <: Dim: NDSpace](kernel: PDKernel[D],
    xs: IndexedSeq[Point[D]],
    D: Double,
    stoppingCriterion: StoppingCriterion) = {

    def k(x: Point[D], y: Point[D]): Double = kernel(x, y)

    extractEigenvalues(computeApproximateEigGeneric[Point[D]](k, xs, D, stoppingCriterion))

  }
}