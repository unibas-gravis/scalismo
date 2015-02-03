package scalismo.image.filter

import scalismo.common.BoxDomain
import scalismo.geometry._

trait Filter[D <: Dim] extends Function1[Point[D], Float] {
  def support: BoxDomain[D]
}

case class GaussianFilter1D(stddev: Double) extends Filter[_1D] {
  def apply(p: Point[_1D]) = {
    val x2 = p(0) * p(0)
    val v = 1.0 / (Math.sqrt((Math.PI * 2)) * stddev) * Math.exp(-x2 / (2 * stddev * stddev))
    v.toFloat
    //    val d= breeze.stats.distributions.Gaussian.distribution(0, stddev*stddev)
    //    d.pdf(p(0))
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxDomain[_1D](Point(-extent), Point(extent))
}

case class GaussianFilter2D(stddev: Double) extends Filter[_2D] {
  def apply(p: Point[_2D]) = {
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1)) / (2 * stddev * stddev))) / (Math.PI * 2 * stddev * stddev)
    v.toFloat
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxDomain[_2D](Point(-extent, -extent), Point(extent, extent))
}

case class GaussianFilter3D(stddev: Double) extends Filter[_3D] {
  def apply(p: Point[_3D]) = {
    val stddev2 = stddev * stddev
    val stddev6 = stddev2 * stddev2 * stddev2

    val PI3 = Math.PI * Math.PI * Math.PI
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1) + p(2) * p(2)) / (2 * stddev2))) / Math.sqrt(8 * PI3 * stddev6)
    v.toFloat
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxDomain[_3D](Point(-extent, -extent, -extent), Point(extent, extent, extent))
}

case class BoxedFilter1D(width: Int) extends Filter[_1D] {
  val w = width / 2f
  def apply(p: Point[_1D]) = {
    if (p(0) < w && p(0) > -w) width 
    else if( p(0).abs == w) w
    else 0f
  }
  def support = BoxDomain[_1D](Point(-w), Point(w))
}

case class BoxedFilter2D(width: Int) extends Filter[_2D] {
  def apply(p: Point[_2D]) = if (support.isDefinedAt(p)) 1f else 0f
  val w = width / 2f
  def support = BoxDomain[_2D](Point(-w, -w), Point(w, w))
}

case class BoxedFilter3D(width: Int) extends Filter[_3D] {
  def apply(p: Point[_3D]) = if (support.isDefinedAt(p)) 1f else 0f
  val w = width / 2f
  def support = BoxDomain[_3D](Point(-w, -w, -w), Point(w, w, w))
}
