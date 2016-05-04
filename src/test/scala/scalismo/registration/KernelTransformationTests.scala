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
package scalismo.registration

import breeze.linalg.DenseMatrix
import scalismo.ScalismoTestSuite
import scalismo.common.BoxDomain
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.geometry.Vector._
import scalismo.image.{ DifferentiableScalarImage, DiscreteImageDomain }
import scalismo.kernels.{ DiagonalKernel, GaussianKernel, Kernel }
import scalismo.numerics.{ GridSampler, Integrator, RandomSVD, UniformSampler }
import scalismo.statisticalmodel.LowRankGaussianProcess.Eigenpair

import scala.language.implicitConversions

class KernelTransformationTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  // TODO add a  test for testing the posterior kernel

  describe("The Nystroem approximation of a Kernel matrix") {
    it("Is close enough to a scalar valued kernel matrix") {
      val kernel = DiagonalKernel(GaussianKernel[_1D](20), 1)
      val domain = BoxDomain(-5.0, 195.0)

      val sampler = UniformSampler(domain, 500)

      val eigPairs = Kernel.computeNystromApproximation[_1D, Vector[_1D]](kernel, 100, sampler)

      def approxKernel(x: Point[_1D], y: Point[_1D]) = {
        eigPairs.indices.foldLeft(0.0)((sum, i) => {
          val Eigenpair(lambda_i, phi_i) = eigPairs(i)
          sum + lambda_i * phi_i(x)(0) * phi_i(y)(0)
        })
      }

      for ((x, _) <- sampler.sample; (y, _) <- sampler.sample) {
        val v1 = kernel(x, y)(0, 0)
        val v2 = approxKernel(x, y)
        v2 should be(v1 +- 0.01)
      }
    }

    it("Its eigenvalues are close enough to the real eigenvalues for 1D") {
      val kernelDim = 1
      val scalarKernel = DiagonalKernel(GaussianKernel[_1D](10), 1)
      val domain = BoxDomain(0.0, 10.0)
      val numPoints = 500
      val sampler = UniformSampler(domain, numPoints)
      val (points, _) = sampler.sample.unzip
      val eigPairsApprox = Kernel.computeNystromApproximation[_1D, Vector[_1D]](scalarKernel, 10, sampler)
      val approxLambdas = eigPairsApprox.map(_.eigenvalue)

      val realKernelMatrix = DenseMatrix.zeros[Double](numPoints * kernelDim, numPoints * kernelDim)

      for (i <- 0 until numPoints; j <- 0 until numPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = scalarKernel(points(i), points(j))(di, dj)
      }

      //val (_,realrealLambdas,_) = breeze.linalg.svd(realKernelMatrix)
      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix * (1.0 / numPoints), eigPairsApprox.size)

      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2) +- 0.1)

    }

    it("Its eigenvalues are close enough to the real eigenvalues in 2D") {

      val kernelDim = 2
      val scalarKernel = GaussianKernel[_2D](10)
      val ndKernel = DiagonalKernel(scalarKernel, kernelDim)
      val domain = BoxDomain((0.0, 0.0), (5.0, 5.0))
      val sampler = UniformSampler(domain, 400)
      val (pts, _) = sampler.sample.unzip

      val eigPairsApprox = Kernel.computeNystromApproximation[_2D, Vector[_2D]](ndKernel, 10, sampler)
      val approxLambdas = eigPairsApprox.map(_.eigenvalue)

      val realKernelMatrix = DenseMatrix.zeros[Double](pts.size * kernelDim, pts.size * kernelDim)

      for (i <- pts.indices; j <- pts.indices; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = ndKernel(pts(i), pts(j))(di, dj)
      }

      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix * (1.0 / pts.size), eigPairsApprox.size)
      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2) +- 0.1)

    }

    it("It leads to orthogonal basis functions on the domain (-5, 5)") {
      val kernel = DiagonalKernel(GaussianKernel[_1D](1.0), 1)
      val domain = BoxDomain(-5.0, 5.0)
      val grid = DiscreteImageDomain(domain.origin, domain.extent * (1.0 / 1000.0), IntVector(1000))
      val sampler = GridSampler(grid)

      val eigPairs = Kernel.computeNystromApproximation[_1D, Vector[_1D]](kernel, 100, sampler)

      val integrator = Integrator(sampler)

      for (i <- 0 until 20) {

        val Eigenpair(_, phi_i) = eigPairs(i)
        def p(x: Point[_1D]) = 1.0 / domain.volume // the eigenfunction is orthogonal with respect to the measure p(x) (from the sampler)
        val phiImg = DifferentiableScalarImage(domain, (x: Point[_1D]) => phi_i(x)(0) * phi_i(x)(0) * p(x), (pt: Point[_1D]) => Vector(0.0))

        val v = integrator.integrateScalar(phiImg)
        v should be(1f +- 0.1)
      }
    }
  }
}
