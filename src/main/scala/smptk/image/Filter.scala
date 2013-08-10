package smptk.image

import smptk.common.BoxedDomain
import smptk.common.BoxedDomain1D
import smptk.common.BoxedDomain2D
import smptk.common.BoxedDomain3D
import smptk.geometry._

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
    val v = (Math.exp(-((p(0) * p(0) + p(1) * p(1)) / (2 * stddev * stddev)))) / (Math.PI * 2 * stddev * stddev)
    v.toFloat
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxedDomain2D(Point2D(-extent, -extent), Point2D(extent, extent))
}



case class GaussianFilter3D(stddev: Double) extends Filter[ThreeD] {
  def apply(p: Point[ThreeD]) = {
    val stddev2 =  stddev * stddev 
    val stddev6 =  stddev2 * stddev2 * stddev2

    val PI3= Math.PI *Math.PI *Math.PI 
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1)+  p(2) * p(2)) / (2 * stddev2)))  / Math.sqrt(8*PI3*stddev6)
    v.toFloat
  }

  val extent = (3.0 * stddev).toFloat
  def support = BoxedDomain3D(Point3D(-extent, -extent, -extent), Point3D(extent, extent, extent))
}

case class BoxedFilter1D() extends Filter[OneD] {
  def apply(p: Point[OneD]) = 1f
  def support = BoxedDomain1D(Point1D(-0.5f), Point1D(0.5f))
}

case class BoxedFilter2D() extends Filter[TwoD] {
  def apply(p: Point[TwoD]) = 1f
  def support = BoxedDomain2D(Point2D(-0.5f, -0.5f), Point2D(0.5f,0.5f))
}

case class BoxedFilter3D() extends Filter[ThreeD] {
  def apply(p: Point[ThreeD]) = 1f
  def support = BoxedDomain3D(Point3D(-0.5f, -0.5f, -0.5f), Point3D(0.5f,0.5f,0.5f))
}
