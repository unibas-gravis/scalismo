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

import breeze.linalg.DenseMatrix
import breeze.linalg.svd.SVD
import scalismo.ScalismoTestSuite
import scalismo.geometry.{_1D, Point}
import scalismo.kernels.{DiagonalKernel, GaussianKernel, Kernel}
import scalismo.utils.Random

class RandomSVDTest extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("The random svd") {

    it("accurately approximates the first 10 eigenvectors and eigenvalues of a gaussian kernel matrix") {

      val k = DiagonalKernel(GaussianKernel[_1D](100), 1)
      val xs = (0 until 1000).map(x => Point(x))

      val K = Kernel.computeKernelMatrix(xs, k)
      val Kdouble = K.map(_.toDouble)
      val (ur, lr, vrt) = RandomSVD.computeSVD(Kdouble, 10)
      val SVD(ub, lb, vbt) = breeze.linalg.svd(Kdouble)

      val mr = ur(::, 0 until 10) * breeze.linalg.diag(lr(0 until 10)) * vrt(0 until 10, 0 until 10)
      val mb = ub(::, 0 until 10) * breeze.linalg.diag(lb(0 until 10)) * vbt(0 until 10, 0 until 10)

      for (j <- 0 until 10; i <- 0 until mr.rows) {
        val factor = Math.max(1e-5, mr(i, j)) / Math.max(1e-5, mb(i, j))
        factor should be(1.0 +- 0.01)
      }
    }

    it("can accurately reproduce the original matrix") {

      // check that the frobenius norm of the reconstruction is exact
      val R = DenseMatrix.rand(100, 100)
      val (u, d, ut) = RandomSVD.computeSVD(R, 100)
      val D = u * breeze.linalg.diag(d) * ut - R
      Math.sqrt(breeze.linalg.trace(D * D.t)) should be <= 1e-5
    }

  }
}
