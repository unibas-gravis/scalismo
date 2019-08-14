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

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.BoxDomain3D
import scalismo.geometry.{ Point, _1D, _3D }
import scalismo.kernels.{ DiagonalKernel, GaussianKernel, Kernel }
import scalismo.utils.Random

class PivotedCholeskyTest extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("The Pivoted Cholesky ") {

    it("accurately approximates a covariance matrix from a random set of points and a kernel k in 1D") {

      val pts = DenseVector.rand[Double](60).toArray.map(v => Point(v.toFloat))
      val k = GaussianKernel[_1D](1.0)
      val matrixValuedK = DiagonalKernel[_1D](k, 1)
      val m = Kernel.computeKernelMatrix[_1D](pts, matrixValuedK)
      val eigCholesky = PivotedCholesky.computeApproximateEig(matrixValuedK, pts, PivotedCholesky.RelativeTolerance(1e-15))
      val (u, d) = eigCholesky
      val D = (u * breeze.linalg.diag(d) * u.t) - m
      Math.sqrt(breeze.linalg.trace(D * D.t)) should be <= 1e-5
    }

    it("accurately approximates a covariance matrix from a random set of points and a kernel k in 3D") {

      val boxDomain = BoxDomain3D(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0))
      val uniformSampler = UniformSampler[_3D](boxDomain, 20)
      val pts = uniformSampler.sample.map(_._1)
      val k = GaussianKernel[_3D](1.0)
      val matrixValuedK = DiagonalKernel[_3D](k, 3)
      val m = Kernel.computeKernelMatrix[_3D](pts, matrixValuedK)
      val eigCholesky = PivotedCholesky.computeApproximateEig(matrixValuedK, pts, PivotedCholesky.RelativeTolerance(1e-15))
      val (u, d) = eigCholesky
      val D = (u * breeze.linalg.diag(d) * u.t) - m
      Math.sqrt(breeze.linalg.trace(D * D.t)) should be <= 1e-5
    }

  }

}
