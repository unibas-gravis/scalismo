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
import scalismo.image.{ DifferentiableScalarImage, DiscreteImageDomain }
import scalismo.kernels.{ GaussianKernel, Kernel, UncorrelatedKernel }
import scalismo.numerics.{ GridSampler, Integrator, RandomSVD, UniformSampler }
import scalismo.statisticalmodel.LowRankGaussianProcess.Eigenpair

import scala.language.implicitConversions

class KernelTransformationTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  // TODO add a  test for testing the posterior kernel

  describe("The Nystroem approximation of a Kernel matrix") {
    it("Is close enough to a scalar valued kernel matrix") {
      val kernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](20))
      val domain = BoxDomain[_1D](-5.0f, 195.0f)

      val sampler = UniformSampler(domain, 500)

      val eigPairs = Kernel.computeNystromApproximation(kernel, 100, sampler)

      def approxKernel(x: Point[_1D], y: Point[_1D]) = {
        (0 until eigPairs.size).foldLeft(0.0)((sum, i) => {
          val Eigenpair(lambda_i, phi_i) = eigPairs(i)
          sum + lambda_i * phi_i(x)(0) * phi_i(y)(0)
        })
      }

      for ((x, _) <- sampler.sample; (y, _) <- sampler.sample) {
        val v1 = kernel(x, y)(0, 0)
        val v2 = approxKernel(x, y)
        v2.toFloat should be(v1 +- 0.01f)
      }
    }

    it("Its eigenvalues are close enough to the real eigenvalues for 1D") {
      val kernelDim = 1
      val scalarKernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](10))
      val domain = BoxDomain[_1D](0.0f, 10.0f)
      val numPoints = 500
      val sampler = UniformSampler(domain, numPoints)
      val (points, _) = sampler.sample.unzip
      val eigPairsApprox = Kernel.computeNystromApproximation(scalarKernel, 10, sampler)
      val approxLambdas = eigPairsApprox.map(_.eigenvalue)

      val realKernelMatrix = DenseMatrix.zeros[Double](numPoints * kernelDim, numPoints * kernelDim)

      for (i <- 0 until numPoints; j <- 0 until numPoints; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = scalarKernel(points(i), points(j))(di, dj)
      }

      //val (_,realrealLambdas,_) = breeze.linalg.svd(realKernelMatrix)
      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix * (1.0 / numPoints), eigPairsApprox.size)

      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2).toFloat +- 0.1f)

    }

    it("Its eigenvalues are close enough to the real eigenvalues in 2D") {

      val kernelDim = 2
      val scalarKernel = GaussianKernel[_2D](10)
      val ndKernel = UncorrelatedKernel[_2D](scalarKernel)
      val domain = BoxDomain[_2D]((0.0f, 0.0f), (5.0f, 5.0f))
      val sampler = UniformSampler(domain, 400)
      val (pts, _) = sampler.sample.unzip

      val eigPairsApprox = Kernel.computeNystromApproximation(ndKernel, 10, sampler)
      val approxLambdas = eigPairsApprox.map(_.eigenvalue)

      val realKernelMatrix = DenseMatrix.zeros[Double](pts.size * kernelDim, pts.size * kernelDim)

      for (i <- 0 until pts.size; j <- 0 until pts.size; di <- 0 until kernelDim; dj <- 0 until kernelDim) {
        realKernelMatrix(i * kernelDim + di, j * kernelDim + dj) = ndKernel(pts(i), pts(j))(di, dj)
      }

      val (_, realLambdas, _) = RandomSVD.computeSVD(realKernelMatrix * (1.0 / pts.size), eigPairsApprox.size)
      for (l <- approxLambdas.zipWithIndex)
        l._1 should be(realLambdas(l._2).toFloat +- 0.1)

    }

    it("It leads to orthogonal basis functions on the domain (-5, 5)") {
      val kernel = UncorrelatedKernel[_1D](GaussianKernel[_1D](1.0))
      val domain = BoxDomain[_1D](-5.0f, 5.0f)
      val grid = DiscreteImageDomain(domain.origin, domain.extent * (1.0 / 1000.0), Index(1000))
      val sampler = GridSampler(grid)

      val eigPairs = Kernel.computeNystromApproximation(kernel, 100, sampler)

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
