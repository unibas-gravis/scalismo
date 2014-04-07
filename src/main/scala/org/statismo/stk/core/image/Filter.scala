package org.statismo.stk.core.image

import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.common.BoxedDomain1D
import org.statismo.stk.core.common.BoxedDomain2D
import org.statismo.stk.core.common.BoxedDomain3D
import org.statismo.stk.core.geometry._

trait Filter[D <: Dim] extends Function1[Point[D], Float] {
  def support: BoxedDomain[D]
}

case class GaussianFilter1D(stddev: Double) extends Filter[OneD] {
  def apply(p: Point[OneD]) = {
    val x2 = p(0) * p(0)
    val v = 1.0 / (Math.sqrt((Math.PI * 2)) * stddev) * Math.exp(-x2 / (2 * stddev * stddev))
    v.toFloat
    //    val d= breeze.stats.distributions.Gaussian.distribution(0, stddev*stddev)
    //    d.pdf(p(0))
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxedDomain1D(Point1D(-extent), Point1D(extent))
}

case class GaussianFilter2D(stddev: Double) extends Filter[TwoD] {
  def apply(p: Point[TwoD]) = {
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1)) / (2 * stddev * stddev))) / (Math.PI * 2 * stddev * stddev)
    v.toFloat
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxedDomain2D(Point2D(-extent, -extent), Point2D(extent, extent))
}

case class GaussianFilter3D(stddev: Double) extends Filter[ThreeD] {
  def apply(p: Point[ThreeD]) = {
    val stddev2 = stddev * stddev
    val stddev6 = stddev2 * stddev2 * stddev2

    val PI3 = Math.PI * Math.PI * Math.PI
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1) + p(2) * p(2)) / (2 * stddev2))) / Math.sqrt(8 * PI3 * stddev6)
    v.toFloat
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxedDomain3D(Point3D(-extent, -extent, -extent), Point3D(extent, extent, extent))
}

case class BoxedFilter1D(width: Int) extends Filter[OneD] {
  val w = width / 2f
  def apply(p: Point[OneD]) = {
    if (p(0) < w && p(0) > -w) width 
    else if( p(0).abs == w) w
    else 0f
  }
  def support = BoxedDomain1D(Point1D(-w), Point1D(w))
}

case class BoxedFilter2D(width: Int) extends Filter[TwoD] {
  def apply(p: Point[TwoD]) = if (support.isDefinedAt(p)) 1f else 0f
  val w = width / 2f
  def support = BoxedDomain2D(Point2D(-w, -w), Point2D(w, w))
}

case class BoxedFilter3D(width: Int) extends Filter[ThreeD] {
  def apply(p: Point[ThreeD]) = if (support.isDefinedAt(p)) 1f else 0f
  val w = width / 2f
  def support = BoxedDomain3D(Point3D(-w, -w, -w), Point3D(w, w, w))
}
