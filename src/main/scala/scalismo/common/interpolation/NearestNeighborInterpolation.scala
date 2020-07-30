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

import scalismo.common.{DiscreteDomain, DiscreteField, EuclideanSpace, Field, RealSpace}
import scalismo.geometry.{_1D, _2D, _3D, Point}

/**
 * Nearest neighbor interpolation of a discrete field. This implementation is generic and
 * works for any discrete field.
 */
case class NearestNeighborInterpolator[D, DDomain[DD] <: DiscreteDomain[DD], A]()
    extends FieldInterpolator[D, DDomain, A] {

  override def interpolate(df: DiscreteField[D, DDomain, A]): Field[D, A] = {

    val pointSet = df.domain.pointSet

    def valueAtClosestPoint(p: Point[D]): A = {
      val closestPointId = pointSet.findClosestPoint(p).id
      df(closestPointId)
    }

    Field(EuclideanSpace[D], valueAtClosestPoint)
  }

}

object NearestNeighborInterpolator1D {
  def apply[DDomain[D] <: DiscreteDomain[D], A](): NearestNeighborInterpolator[_1D, DDomain, A] =
    NearestNeighborInterpolator[_1D, DDomain, A]()
}

object NearestNeighborInterpolator2D {
  def apply[DDomain[D] <: DiscreteDomain[D], A](): NearestNeighborInterpolator[_2D, DDomain, A] =
    NearestNeighborInterpolator[_2D, DDomain, A]()
}
object NearestNeighborInterpolator3D {
  def apply[DDomain[D] <: DiscreteDomain[D], A](): NearestNeighborInterpolator[_3D, DDomain, A] =
    NearestNeighborInterpolator[_3D, DDomain, A]()
}
