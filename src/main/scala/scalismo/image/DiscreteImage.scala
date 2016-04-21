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
package scalismo.image

import breeze.linalg.DenseVector
import scalismo.common._

import scalismo.common.{ RealSpace, ScalarField, DiscreteScalarField, DiscreteField }
import scalismo.geometry._
import scalismo.image
import scalismo.image.DiscreteScalarImage.Create
import scalismo.numerics.BSpline
import scala.reflect.ClassTag

/**
 * Basic interface for a discrete image of arbitrary Pixel type
 *
 * @tparam D  The dimensionality of the image
 * @tparam A The type of the pixel (usually a scalar or a vector)
 */
class DiscreteImage[D <: Dim: NDSpace, A](domain: DiscreteImageDomain[D], values: IndexedSeq[A]) extends DiscreteField[D, A](domain, values) {

  protected[this] def ndSpace: NDSpace[D] = NDSpace[D]

  //  override def domain: DiscreteImageDomain[D]

  val dimensionality = ndSpace.dimensionality

  def apply(idx: IntVector[D]): A = this(domain.pointId(idx))

  def isDefinedAt(idx: IntVector[D]): Boolean = {
    (0 until dimensionality).foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < domain.size(d))
  }

}

