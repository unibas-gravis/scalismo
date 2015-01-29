package org.statismo.stk.core.kernels

import org.statismo.stk.core.common.{RealSpace, ImmutableLRU}
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.numerics.BSpline
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.utils.Memoize

/**
* Created by luetma00 on 15.01.15.
*/


case class GaussianKernel[D <: Dim](val sigma: Double) extends PDKernel[D] {
  val sigma2 = sigma * sigma

  override def domain = RealSpace[D]

  def apply(x: Point[D], y: Point[D]) = {
    val r = x - y
    scala.math.exp(-r.norm2 / sigma2)
  }
}


case class SampleCovarianceKernel[D <: Dim : NDSpace](val ts: IndexedSeq[Transformation[D]], cacheSizeHint: Int = 100000) extends MatrixValuedPDKernel[D, D] {

  override def domain = ts.headOption.map(ts => ts.domain).getOrElse(RealSpace[D])

  val ts_memoized = for (t <- ts) yield Memoize(t, cacheSizeHint)

  def mu(x: Point[D]): Vector[D] = {
    var meanDisplacement = Vector.zeros[D]
    var i = 0;
    while (i < ts.size) {
      val t = ts_memoized(i)
      meanDisplacement = meanDisplacement + (t(x) - x)
      i += 1
    }
    meanDisplacement * (1.0 / ts.size)
  }


  val mu_memoized = Memoize(mu, cacheSizeHint)

  def apply(x: Point[D], y: Point[D]): SquareMatrix[D] = {
    var ms = SquareMatrix.zeros[D]
    var i = 0;
    while (i < ts.size) {
      val t = ts_memoized(i)
      val ux = t(x) - x
      val uy = t(y) - y
      ms = ms + (ux - mu_memoized(x)).outer(uy - mu_memoized(y))
      i += 1
    }
    ms * (1f / (ts.size - 1))
  }

}




abstract case class BSplineKernel[D <: Dim ](order : Int, scale : Int) extends PDKernel[D] {
  override def domain = RealSpace[D]

}

object BSplineKernel {

  def apply[D <: Dim : CanCreate](order: Int, scale: Int) : BSplineKernel[D] = {
    implicitly[CanCreate[D]].create(order, scale)
  }

  trait CanCreate[D <: Dim] {
    def create(order: Int, j: Int): BSplineKernel[D]
  }


  implicit object CanCreateBSplineKernel1D extends CanCreate[_1D] {
    def create(order: Int, j: Int) : BSplineKernel[_1D] = new BSplineKernel1D(order, j)
  }

  implicit object CanCreateBSplineKernel2D extends CanCreate[_2D] {
    def create(order: Int, j: Int) : BSplineKernel[_2D] = new BSplineKernel2D(order, j)
  }

  implicit object CanCreateBSplineKernel3D extends CanCreate[_3D] {
    def create(order: Int, j: Int) : BSplineKernel[_3D] = new BSplineKernel3D(order, j)
  }


  private class BSplineKernel3D(order: Int, scale: Int) extends BSplineKernel[_3D](order, scale) {

    val spline = BSpline.nthOrderBSpline(order) _

    def bspline3D(x1: Float, x2: Float, x3: Float) = {
        spline(x1) * spline(x2) * spline(x3)
    }

    val c: Double = scala.math.pow(2.0, scale)
    val O: Double = 0.5 * (order + 1)
    val two_j: Float = c.toFloat

    def apply(x: Point[_3D], y: Point[_3D]) = {

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

            sum_j = sum_j + (bspline3D(xVec_j(0) - kx, xVec_j(1) - ky, xVec_j(2) - kz) * bspline3D(yVec_j(0) - kx, yVec_j(1) - ky, yVec_j(2) - kz))

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
    def bspline2D(x1: Float, x2: Float) = {
      spline(x1) * spline(x2)
    }

    val c: Double = scala.math.pow(2.0, scale)
    val O: Double = 0.5 * (order + 1)
    val two_j: Float = c.toFloat

    def apply(x: Point[_2D], y: Point[_2D]) = {

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

    val bspline1D =  BSpline.nthOrderBSpline(order) _

    val c: Double = scala.math.pow(2.0, scale)
    val O: Double = 0.5 * (order + 1)
    val two_j: Float = c.toFloat

    def apply(x: Point[_1D], y: Point[_1D]) = {

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

}


