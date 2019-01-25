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
package scalismo.image.filter

import scalismo.common.BoxDomain
import scalismo.geometry._

/**
 * Trait for filters to be used in a convolution
 */
trait Filter[D <: Dim] extends Function1[Point[D], Float] {
  /**
   * Returns the continuous domain on which the filter is defined
   */
  def support: BoxDomain[D]
}
/**
 * One dimensional Gaussian Blur Filter to be used in a convolution
 * @constructor
 * @param stddev Standard deviation of the Gaussian to be used.
 * The extent of the support of the Filter is fixed to be 6 times the standard deviation (3 stddev on each direction)
 */
case class GaussianFilter1D(stddev: Double) extends Filter[_1D] {
  def apply(p: Point[_1D]) = {
    val x2 = p(0) * p(0)
    val v = 1.0 / (Math.sqrt((Math.PI * 2)) * stddev) * Math.exp(-x2 / (2 * stddev * stddev))
    v.toFloat
  }

  val radius = (3.0 * stddev).toFloat
  def support = BoxDomain(Point(-radius), Point(radius))
}

/**
 * 2 dimensional Gaussian blur filter. See [[GaussianFilter1D]]
 */
case class GaussianFilter2D(stddev: Double) extends Filter[_2D] {
  def apply(p: Point[_2D]) = {
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1)) / (2 * stddev * stddev))) / (Math.PI * 2 * stddev * stddev)
    v.toFloat
  }

  val radius = (3.0 * stddev).toFloat
  def support = BoxDomain(Point(-radius, -radius), Point(radius, radius))
}
/**
 * 3 dimensional Gaussian blur filter. See [[GaussianFilter1D]]
 */
case class GaussianFilter3D(stddev: Double) extends Filter[_3D] {
  def apply(p: Point[_3D]) = {
    val stddev2 = stddev * stddev
    val stddev6 = stddev2 * stddev2 * stddev2

    val PI3 = Math.PI * Math.PI * Math.PI
    val v = Math.exp(-((p(0) * p(0) + p(1) * p(1) + p(2) * p(2)) / (2 * stddev2))) / Math.sqrt(8 * PI3 * stddev6)
    v.toFloat
  }

  val radius = (3.0 * stddev).toFloat
  def support = BoxDomain(Point(-radius, -radius, -radius), Point(radius, radius, radius))
}
/**
 * D- dimensional box Blurring Filter to be used in a convolution. The filter has a value 1 in its support and 0 otherwise
 * @constructor
 * @param width Defines the width of the filter support
 */

case class BoxedFilter[D <: Dim: NDSpace](width: Double) extends Filter[D] {
  def apply(p: Point[D]) = if (support.isDefinedAt(p)) 1f else 0f
  val w = width / 2.0
  val v = EuclideanVector[D](breeze.linalg.DenseVector.ones[Double](implicitly[NDSpace[D]].dimensionality).data)

  def support = BoxDomain[D]((v * (-w)).toPoint, (v * (w)).toPoint)
}

