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

import breeze.linalg.DenseMatrix
import scalismo.common.{ PointId, DiscreteDomain }
import scalismo.geometry.{ NDSpace, Dim, SquareMatrix }

/**
 *  Discrete representation of a MatrixValuedPDKernel.
 *  Mathematically, it can be represented as a covariance matrix. However, it has more structure, i.e. its entry ij
 *  is a matrix. Furthermore, the class has the knowledge about its domain (the point on which it is defined).
 */
class DiscreteMatrixValuedPDKernel[D <: Dim: NDSpace, DO <: Dim: NDSpace] private[scalismo] (val domain: DiscreteDomain[D],
    val k: (PointId, PointId) => SquareMatrix[DO]) {
  self =>

  def outputDim = implicitly[NDSpace[DO]].dimensionality

  def apply(i: PointId, j: PointId): SquareMatrix[DO] = {
    if (i.id < domain.numberOfPoints && j.id < domain.numberOfPoints)
      k(i, j)
    else {
      if (i.id >= domain.numberOfPoints) {
        throw new IllegalArgumentException((s"$i is not a valid index"))
      } else {
        throw new IllegalArgumentException((s"$j is not a valid index"))
      }
    }
  }

  /**
   * return the matrix representation of this kernel.
   * (This is a covariance matrix, consisting of blocks of size DO times DO)
   */
  def asBreezeMatrix: DenseMatrix[Float] = {
    val d = outputDim
    val xs = domain.points.toIndexedSeq

    val K = DenseMatrix.zeros[Float](xs.size * d, xs.size * d)
    val xiWithIndex = xs.zipWithIndex.par
    val xjWithIndex = xs.zipWithIndex
    for { i <- 0 until xs.size; j <- 0 to i } {
      val kxixj = k(PointId(i), PointId(j))
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

}

object DiscreteMatrixValuedPDKernel {
  def apply[D <: Dim: NDSpace, DO <: Dim: NDSpace](domain: DiscreteDomain[D], k: (PointId, PointId) => SquareMatrix[DO]) = {
    new DiscreteMatrixValuedPDKernel(domain, k)
  }
}
