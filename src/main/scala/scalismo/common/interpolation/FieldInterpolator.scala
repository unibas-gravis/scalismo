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

import scalismo.common.{ DiscreteDomain, DiscreteField, Field }
import scalismo.geometry.Dim

/**
 * Base trait for all interpolators that can be used to interpolate a [[DiscreteField]]
 * @tparam D Dimensionality
 * @tparam DDomain Type of the [[DiscreteDomain]] that the interpolator can interpolate
 * @tparam A The value type
 */
trait FieldInterpolator[D <: Dim, -DDomain <: DiscreteDomain[D], A] {

  /**
   * Interpolates a given discrete field using the given interpolator.
   * @param df the discrete field to be interpolated
   * @return A continuous field of the same type.
   */
  def interpolate(df: DiscreteField[D, DDomain, A]): Field[D, A]
}
