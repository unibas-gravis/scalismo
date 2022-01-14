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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.EuclideanSpace
import scalismo.geometry._
import scalismo.numerics.BSpline
import scalismo.transformations.Transformation
import scalismo.utils.Memoize

case class GaussianKernel[D](sigma: Double) extends PDKernel[D] {
  val sigma2 = sigma * sigma

  override def domain = EuclideanSpace[D]

  override def k(x: Point[D], y: Point[D]): Double = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}

object GaussianKernel1D {
  def apply(sigma: Double): GaussianKernel[_1D] = GaussianKernel[_1D](sigma)
}

object GaussianKernel2D {
  def apply(sigma: Double): GaussianKernel[_2D] = GaussianKernel[_2D](sigma)
}

object GaussianKernel3D {
  def apply(sigma: Double): GaussianKernel[_3D] = GaussianKernel[_3D](sigma)
}

case class SampleCovarianceKernel[D: NDSpace](ts: IndexedSeq[Transformation[D]], cacheSizeHint: Int = 100000)
    extends MatrixValuedPDKernel[D] {

  override val outputDim = NDSpace[D].dimensionality

  override val domain = ts.headOption.map(ts => ts.domain).getOrElse(EuclideanSpace[D])

  private val ts_memoized = for (t <- ts) yield Memoize(t, cacheSizeHint)
  private val normFactorMu = (1.0 / (ts.size))
  private val normFactorCov = (1.0 / (ts.size - 1))

  def mu(x: Point[D]): DenseVector[Double] = {

    val meanDisplacement = DenseVector.zeros[Double](outputDim)

    for (t <- ts_memoized) {
      var i = 0;
      val tx = t(x)
      while (i < outputDim) {
        meanDisplacement(i) += tx(i) - x(i)
        i += 1
      }
    }
    meanDisplacement * normFactorMu
  }

  private val mu_memoized = Memoize(mu, cacheSizeHint)

  override def k(x: Point[D], y: Point[D]): DenseMatrix[Double] = {
    val ms = DenseMatrix.zeros[Double](outputDim, outputDim)

    // cache the mean values as computation of these takes time
    val mux = mu_memoized(x)
    val muy = mu_memoized(y)

    for (t <- ts_memoized) {
      // transformed points
      val tx = t(x)
      val ty = t(y)

      // the corresponding deformations
      val ux = tx - x
      val uy = ty - y

      // the following two loops compute the outer product of (ux - mux)(uy - muy).
      // since the matrices are tiny, it seems to be most efficient to do the computation by hand,
      // and thus avoid allocating many small objects.
      var i = 0
      while (i < outputDim) {
        var j = 0

        while (j < outputDim) {

          ms(i, j) = ms(i, j) + (ux(i) - mux(i)) * (uy(j) - muy(j))
          j += 1
        }
        i += 1

      }
    }
    ms * normFactorCov
  }

}

abstract case class BSplineKernel[D](order: Int, scale: Int) extends PDKernel[D] {
  override def domain = EuclideanSpace[D]

}

trait CreateBSplineKernel[D] {
  def create(order: Int, j: Int): BSplineKernel[D]
}

object CreateBSplineKernel {

  implicit object CreateBSplineKernelBSplineKernel1D extends CreateBSplineKernel[_1D] {
    def create(order: Int, j: Int): BSplineKernel[_1D] = new BSplineKernel1D(order, j)
  }

  implicit object CreateBSplineKernelBSplineKernel2D extends CreateBSplineKernel[_2D] {
    def create(order: Int, j: Int): BSplineKernel[_2D] = new BSplineKernel2D(order, j)
  }

  implicit object CreateBSplineKernelBSplineKernel3D extends CreateBSplineKernel[_3D] {
    def create(order: Int, j: Int): BSplineKernel[_3D] = new BSplineKernel3D(order, j)
  }

}

object BSplineKernel {

  def apply[D: CreateBSplineKernel](order: Int, scale: Int): BSplineKernel[D] = {
    implicitly[CreateBSplineKernel[D]].create(order, scale)
  }

}

class BSplineKernel3D(order: Int, scale: Int) extends BSplineKernel[_3D](order, scale) {

  val spline = BSpline.nthOrderBSpline(order) _

  def bspline3D(x1: Double, x2: Double, x3: Double) = {
    spline(x1) * spline(x2) * spline(x3)
  }

  val c: Double = scala.math.pow(2.0, scale)
  val O: Double = 0.5 * (order + 1)
  val two_j: Double = c

  override def k(x: Point[_3D], y: Point[_3D]) = {

    // Sum over all j from low to up

    val kl_x: Int = scala.math.ceil(scala.math.max(x(0), y(0)) * c - O).toInt
    val kl_y: Int = scala.math.ceil(scala.math.max(x(1), y(1)) * c - O).toInt
    val kl_z: Int = scala.math.ceil(scala.math.max(x(2), y(2)) * c - O).toInt

    val kll_x = scala.math.min(x(0), y(0)) * c - O
    val kll_y = scala.math.min(x(1), y(1)) * c - O
    val kll_z = scala.math.min(x(2), y(2)) * c - O

    val ku_x: Int = scala.math.floor(kll_x + order + 1).toInt
    val ku_y: Int = scala.math.floor(kll_y + order + 1).toInt
    val ku_z: Int = scala.math.floor(kll_z + order + 1).toInt

    val xVec_j = x.toVector * two_j
    val yVec_j = y.toVector * two_j

    var sum_j: Double = 0.0

    var kx = kl_x
    while (kx <= ku_x) {
      var ky = kl_y
      while (ky <= ku_y) {
        var kz = kl_z
        while (kz <= ku_z) {

          sum_j = sum_j + (bspline3D(xVec_j(0) - kx, xVec_j(1) - ky, xVec_j(2) - kz) * bspline3D(yVec_j(0) - kx,
                                                                                                 yVec_j(1) - ky,
                                                                                                 yVec_j(2) - kz))

          kz = kz + 1
        }
        ky = ky + 1
      }
      kx = kx + 1
    }

    sum_j

    // Compute bounding box to use compact support properties.

  }

}

private class BSplineKernel2D(order: Int, scale: Int) extends BSplineKernel[_2D](order, scale) {

  private val spline = BSpline.nthOrderBSpline(order) _
  def bspline2D(x1: Double, x2: Double) = {
    spline(x1) * spline(x2)
  }

  val c: Double = scala.math.pow(2.0, scale)
  val O: Double = 0.5 * (order + 1)
  val two_j: Double = c

  override def k(x: Point[_2D], y: Point[_2D]) = {

    // Sum over all j from low to up

    val kl_x: Int = scala.math.ceil(scala.math.max(x(0), y(0)) * c - O).toInt
    val kl_y: Int = scala.math.ceil(scala.math.max(x(1), y(1)) * c - O).toInt

    val kll_x = scala.math.min(x(0), y(0)) * c - O
    val kll_y = scala.math.min(x(1), y(1)) * c - O

    val ku_x: Int = scala.math.floor(kll_x + order + 1).toInt
    val ku_y: Int = scala.math.floor(kll_y + order + 1).toInt

    val xVec_j = x.toVector * two_j
    val yVec_j = y.toVector * two_j

    var sum_j: Double = 0.0

    var kx = kl_x
    while (kx <= ku_x) {
      var ky = kl_y
      while (ky <= ku_y) {

        sum_j = sum_j + bspline2D(xVec_j(0) - kx, xVec_j(1) - ky) * bspline2D(yVec_j(0) - kx, yVec_j(1) - ky)

        ky = ky + 1
      }
      kx = kx + 1
    }

    sum_j

    // Compute bounding box to use compact support properties.

  }
}

private class BSplineKernel1D(order: Int, scale: Int) extends BSplineKernel[_1D](order, scale) {

  val bspline1D = BSpline.nthOrderBSpline(order) _

  val c: Double = scala.math.pow(2.0, scale)
  val O: Double = 0.5 * (order + 1)
  val two_j: Double = c

  override def k(x: Point[_1D], y: Point[_1D]) = {

    // Sum over all j from low to up

    val kl_x: Int = scala.math.ceil(scala.math.max(x(0), y(0)) * c - O).toInt

    val kll_x = scala.math.min(x(0), y(0)) * c - O

    val ku_x: Int = scala.math.floor(kll_x + order + 1).toInt

    val xVec_j = x.toVector * two_j
    val yVec_j = y.toVector * two_j

    var sum_j: Double = 0.0

    var kx = kl_x
    while (kx <= ku_x) {

      sum_j = sum_j + bspline1D(xVec_j(0) - kx) * bspline1D(yVec_j(0) - kx)
      kx = kx + 1

    }

    sum_j

    // Compute bounding box to use compact support properties.

  }
}
