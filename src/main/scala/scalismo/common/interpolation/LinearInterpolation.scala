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

package scalismo.common.interpolation

import scalismo.common.{ DiscreteField, Field }
import scalismo.geometry.{ Dim, IntVector, IntVector2D, IntVector3D, NDSpace, Point, _1D, _2D, _3D }
import scalismo.image.DiscreteImageDomain
import scalismo.numerics.ValueInterpolator

trait LinearImageInterpolator[D <: Dim, A] extends FieldInterpolator[D, DiscreteImageDomain[D], A] {

  protected implicit def ndSpace: NDSpace[D]

  protected implicit def valueInterpolator: ValueInterpolator[A]

  protected def pointToContinuousIndex(domain: DiscreteImageDomain[D], pt: Point[D]): Point[D] = {
    val dim = pt.dimensionality
    val cIdx = (0 until dim)
      .map(i => {
        val cIdx = (pt(i) - domain.origin(i)) / domain.spacing(i)
        // adjust range to size of domain
        Math.min(Math.max(0.0, cIdx), domain.size(i) - 1.0)
      })

    Point[D](cIdx.toArray)
  }

}

object LinearImageInterpolator {

  def apply[D <: Dim, A: ValueInterpolator]()(implicit interpolator: LinearImageInterpolator[D, A]): LinearImageInterpolator[D, A] = interpolator

  implicit def linearImageInterpolator1D[A: ValueInterpolator] = LinearImageInterpolator1D[A]()

  implicit def linearImageInterpolator2D[A: ValueInterpolator] = LinearImageInterpolator2D[A]()

  implicit def linearImageInterpolator3D[A: ValueInterpolator] = LinearImageInterpolator3D[A]()
}

case class LinearImageInterpolator1D[A: ValueInterpolator]() extends LinearImageInterpolator[_1D, A] {

  override protected val ndSpace = NDSpace[_1D]

  override protected val valueInterpolator = ValueInterpolator[A]

  override def interpolate(df: DiscreteField[_1D, DiscreteImageDomain[_1D], A]): Field[_1D, A] = {

    val domain = df.domain

    def valueAtIdx(idx: IntVector[_1D]): A = {
      val id = domain.pointId(idx)
      df(id)
    }

    def interpolatePoint(p: Point[_1D]): A = {
      val ctdIndex = pointToContinuousIndex(domain, p)

      val x0 = ctdIndex(0).floor.toInt
      val x1 = if (x0 != ctdIndex(0)) x0 + 1 else x0
      val xd = (ctdIndex(0) - x0)
      valueInterpolator.convexCombination(
        (valueAtIdx(IntVector(x0)), (1.0f - xd)),
        (valueAtIdx(IntVector(x1)), xd)
      )
    }

    Field(domain.boundingBox, interpolatePoint)
  }
}

case class LinearImageInterpolator2D[A: ValueInterpolator]() extends LinearImageInterpolator[_2D, A] {

  override protected val ndSpace = NDSpace[_2D]

  override protected val valueInterpolator = ValueInterpolator[A]

  override def interpolate(df: DiscreteField[_2D, DiscreteImageDomain[_2D], A]): Field[_2D, A] = {

    val domain = df.domain

    def valueAtIdx(idx: IntVector[_2D]): A = {
      val id = domain.pointId(idx)
      df(id)
    }

    def interpolatePoint(p: Point[_2D]): A = {
      val ctdIndex = pointToContinuousIndex(domain, p)

      val x0 = ctdIndex(0).floor.toInt
      val x1 = if (x0 != ctdIndex(0)) x0 + 1 else x0
      val y0 = ctdIndex(1).floor.toInt
      val y1 = if (y0 != ctdIndex(1)) y0 + 1 else y0
      val xd = (ctdIndex(0) - x0)
      val yd = (ctdIndex(1) - y0)
      val c00 = valueInterpolator.convexCombination(
        (valueAtIdx(IntVector2D(x0, y0)), (1.0f - xd)),
        (valueAtIdx(IntVector2D(x1, y0)), xd)
      )
      val c10 = valueInterpolator.convexCombination(
        (valueAtIdx(IntVector2D(x0, y1)), (1.0 - xd)),
        (valueAtIdx(IntVector2D(x1, y1)), xd)
      )
      valueInterpolator.convexCombination(
        (c00, 1.0 - yd),
        (c10, yd)
      )
    }

    Field(domain.boundingBox, interpolatePoint)
  }
}

case class LinearImageInterpolator3D[A: ValueInterpolator]() extends LinearImageInterpolator[_3D, A] {

  override protected val ndSpace = NDSpace[_3D]

  override protected val valueInterpolator = ValueInterpolator[A]

  override def interpolate(df: DiscreteField[_3D, DiscreteImageDomain[_3D], A]): Field[_3D, A] = {

    val domain = df.domain

    def valueAtIdx(idx: IntVector[_3D]): A = {
      val id = domain.pointId(idx)
      df(id)
    }

    def interpolatePoint(p: Point[_3D]): A = {
      val ctdIndex = pointToContinuousIndex(domain, p)

      val x0 = ctdIndex(0).floor.toInt
      val x1 = if (x0 != ctdIndex(0)) x0 + 1 else x0
      val y0 = ctdIndex(1).floor.toInt
      val y1 = if (y0 != ctdIndex(1)) y0 + 1 else y0
      val z0 = ctdIndex(2).floor.toInt
      val z1 = if (z0 != ctdIndex(2)) z0 + 1 else z0
      val xd = (ctdIndex(0) - x0)
      val yd = (ctdIndex(1) - y0)
      val zd = (ctdIndex(2) - z0)
      val c00 = valueInterpolator.convexCombination(
        (valueAtIdx(IntVector3D(x0, y0, z0)), (1.0 - xd)),
        (valueAtIdx(IntVector3D(x1, y0, z0)), xd)
      )
      val c01 = valueInterpolator.convexCombination(
        (valueAtIdx(IntVector3D(x0, y0, z1)), (1.0 - xd)),
        (valueAtIdx(IntVector3D(x1, y0, z1)), xd)
      )
      val c10 = valueInterpolator.convexCombination(
        (valueAtIdx(IntVector3D(x0, y1, z0)), (1.0 - xd)),
        (valueAtIdx(IntVector3D(x1, y1, z0)), xd)
      )
      val c11 = valueInterpolator.convexCombination(
        (valueAtIdx(IntVector3D(x0, y1, z1)), (1.0 - xd)),
        (valueAtIdx(IntVector3D(x1, y1, z1)), xd)
      )
      val c0 = valueInterpolator.convexCombination((c00, (1.0 - yd)), (c10, yd))
      val c1 = valueInterpolator.convexCombination((c01, (1.0 - yd)), (c11, yd))
      val c = valueInterpolator.convexCombination((c0, (1.0 - zd)), (c1, zd))
      c
    }

    Field(domain.boundingBox, interpolatePoint)
  }
}
