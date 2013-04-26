package smptk.image

import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.Point1D
import smptk.common.BoxedRegion
import smptk.common.BoxedRegion1D
import smptk.image.Geometry.CoordVector2D
import smptk.image.Geometry.Point2D
import smptk.common.BoxedRegion2D
import smptk.image.Geometry.CoordVector3D
import smptk.image.Geometry.Point3D
import smptk.common.BoxedRegion3D

trait Filter[CV[A] <: CoordVector[A]] extends Function1[CV[Double], Double] {
  def support: BoxedRegion[CV]
}

case class GaussianFilter1D(stddev: Double) extends Filter[CoordVector1D] {
  def apply(p: Point1D) = {
    val x2 = p(0) * p(0)
    1. / (Math.sqrt((Math.PI * 2)) * stddev) * Math.exp(-x2 / (2 * stddev * stddev))
    //    val d= breeze.stats.distributions.Gaussian.distribution(0, stddev*stddev)
    //    d.pdf(p(0))
  }

  val extent = 3. * stddev
  def support = BoxedRegion1D(CoordVector1D(-extent), CoordVector1D(extent))
}

case class GaussianFilter2D(stddev: Double) extends Filter[CoordVector2D] {
  def apply(p: Point2D) = {
    (Math.exp(-((p(0) * p(0) + p(1) * p(1)) / (2 * stddev * stddev)))) / (Math.PI * 2 * stddev * stddev)
  }

  val extent = 3. * stddev
  def support = BoxedRegion2D(CoordVector2D(-extent, -extent), CoordVector2D(extent, extent))
}




case class BoxedFilter1D extends Filter[CoordVector1D] {
  def apply(p: Point1D) = 1.
  def support = BoxedRegion1D(CoordVector1D(-0.5), CoordVector1D(0.5))
}

case class BoxedFilter2D extends Filter[CoordVector2D] {
  def apply(p: Point2D) = 1.
  def support = BoxedRegion2D(CoordVector2D(-0.5, -0.5), CoordVector2D(0.5,0.5))
}

case class BoxedFilter3D extends Filter[CoordVector3D] {
  def apply(p: Point3D) = 1.
  def support = BoxedRegion3D(CoordVector3D(-0.5, -0.5, -0.5), CoordVector3D(0.5,0.5,0.5))
}
