package org.statismo.stk.core.kernels

import org.statismo.stk.core.geometry.{OneD, TwoD, Point, ThreeD}

/**
 * Created by gerith00 on 3/26/14.
 */


object MultiscaleKernel {

}

case class MultiScaleBSplineKernel1D(order: Int, min: Int, max: Int) extends PDKernel[OneD] {

  val scale = (j: Int) => scala.math.pow(2.0, -1.0 * j)
  val bspline1D = BSpline.create1DBSpline(order)
  val kernels = for (i <- min to max) yield { BSplineKernel1D(order, i) * scale(i) }

  def apply(x: Point[OneD], y: Point[OneD]): Double = {
    var sum = 0.0
    for (k <- kernels) {
      sum += k(x, y)
    }
    sum
  }

}

case class MultiScaleBSplineKernel2D(order: Int, min: Int, max: Int) extends PDKernel[TwoD] {

  val scale = (j: Int) => scala.math.pow(2.0, -1.0 * j)

  val kernels = for (i <- min to max) yield { BSplineKernel2D(order, i) * scale(i) }

  def apply(x: Point[TwoD], y: Point[TwoD]): Double = {
    var sum = 0.0
    for (k <- kernels) {
      sum += k(x, y)
    }
    sum
  }

}

case class MultiScaleBSplineKernel3D(order: Int, min: Int, max: Int) extends PDKernel[ThreeD] {

  val scale = (j: Int) => scala.math.pow(2.0, -2.0 * j)

  val kernels = for (i <- min to max) yield { BSplineKernel3D(order, i) * scale(i) }

  def apply(x: Point[ThreeD], y: Point[ThreeD]): Double = {
    var sum = 0.0
    for (k <- kernels) {
      sum += k(x, y)
    }
    sum
  }

}

case class AnisotropicMultiScaleBSplineKernel3D(order: Int, min: Int, max: Int, eta: Point[ThreeD] => Double) extends PDKernel[ThreeD] {

  val scale = (x:Point[ThreeD], y: Point[ThreeD], j: Int) => scala.math.pow(2.0, -1*j*((eta(x)+eta(y) + 2.0)*0.5))

  val kernels = for (i <- min to max) yield { BSplineKernel3D(order, i) }

  def apply(x: Point[ThreeD], y: Point[ThreeD]): Double = {
    var sum = 0.0
    for ((k,i) <- kernels.zipWithIndex) {
      sum += k(x, y)*scale(x,y,i)
    }
    sum
  }

}

case class BSplineKernel3D(order: Int, j: Int) extends PDKernel[ThreeD] {

  val bspline3D = BSpline.create3DBSpline(order)
  val c: Double = scala.math.pow(2.0, j)
  val O: Double = 0.5 * (order + 1)
  val two_j: Float = c.toFloat

  def apply(x: Point[ThreeD], y: Point[ThreeD]) = {

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

case class BSplineKernel2D(order: Int, j: Int) extends PDKernel[TwoD] {

  val bspline2D = BSpline.create2DBSpline(order)
  val c: Double = scala.math.pow(2.0, j)
  val O: Double = 0.5 * (order + 1)
  val two_j: Float = c.toFloat

  def apply(x: Point[TwoD], y: Point[TwoD]) = {

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

case class BSplineKernel1D(order: Int, j: Int) extends PDKernel[OneD] {

  val bspline1D = BSpline.create1DBSpline(order)
  val c: Double = scala.math.pow(2.0, j)
  val O: Double = 0.5 * (order + 1)
  val two_j: Float = c.toFloat

  def apply(x: Point[OneD], y: Point[OneD]) = {

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

case class SpatiallyVaryingBSplineKernel1D(order: Int, j: Int, eta: Point[OneD] => Double) extends PDKernel[OneD] {

  val bspline1D = BSpline.create1DBSpline(order)
  val c: Double = scala.math.pow(2.0, j)
  val O: Double = 0.5 * (order + 1)
  val two_j: Float = c.toFloat

  def apply(x: Point[OneD], y: Point[OneD]) = {

    // Sum over all j from low to up

    val kl_x: Int = scala.math.ceil(scala.math.max(x(0), y(0)) * c - O).toInt

    val kll_x = scala.math.min(x(0), y(0)) * c - O

    val ku_x: Int = scala.math.floor(kll_x + order + 1).toInt

    val xVec_j = x.toVector * two_j
    val yVec_j = y.toVector * two_j

    var sum_j: Double = 0.0

    var kx = kl_x
    while (kx <= ku_x) {

      sum_j = sum_j + (eta(x)*bspline1D(xVec_j(0) - kx) * eta(y)*bspline1D(yVec_j(0) - kx))
      kx = kx + 1

    }

    sum_j

    // Compute bounding box to use compact support properties.

  }

}

object BSpline {

  private val twoByThree = 2.0f / 3.0f; // a constant used later on

  def create1DBSpline(n: Int): (Float) => Float = { bSpline(n)(_) }

  def create2DBSpline(n: Int): (Float, Float) => Float = {

    val spline = bSpline(n)(_)

    def bspline2D(x1: Float, x2: Float): Float = {

      spline(x1) * spline(x2)

    }

    bspline2D
  }

  def create3DBSpline(n: Int): (Float, Float, Float) => Float = {

    val spline = bSpline(n)(_)

    def bspline3D(x1: Float, x2: Float, x3: Float): Float = {

      spline(x1) * spline(x2) * spline(x3)

    }

    bspline3D
  }

  def bSpline(n: Int)(x: Float): Float = {
    val absX: Float = scala.math.abs(x)
    val absXSquared: Float = absX * absX
    val absXCube: Float = absXSquared * absX
    val twoMinAbsX: Float = 2.0f - absX

    n match {
      case 0 => {
        if (-0.5f < x && x < 0.5f) 1.0f
        else if (absX == 0.5f) 0.5f
        else 0

      }
      case 1 => {
        if (-1 <= x && x <= 0) 1.0f + x
        else if (0 < x && x <= 1) 1.0f - x
        else 0
      }
      case 2 => {
        if (-1.5 <= x && x < -0.5f) 0.5f * (x + 1.5f) * (x + 1.5f)
        else if (-0.5 <= x && x < 0.5f) -(x + 0.5f) * (x + 0.5f) + (x - 0.5f) + 1.5f
        else if (x >= 0.5 && x < 1.5f) 0.5f * (1 - (x - 0.5f)) * (1 - (x - 0.5f))
        else 0

      }
      case 3 => {

        if (absX >= 0 && absX < 1)
          twoByThree - absXSquared + 0.5f * absXCube
        else if (absX >= 1 && absX < 2)
          twoMinAbsX * twoMinAbsX * twoMinAbsX / 6.0f
        else 0
      }
      case _ => throw new NotImplementedError("Bspline of order " + n + " is not implemented yet")
    }
  }

}
