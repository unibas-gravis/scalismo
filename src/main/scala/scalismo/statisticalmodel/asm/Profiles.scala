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
package scalismo.statisticalmodel.asm

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry.{ Dim, NDSpace, Point }
import scalismo.statisticalmodel.MultivariateNormalDistribution

import scala.collection.immutable

final case class ProfileId(id: Int) extends AnyVal

case class Profile(pointId: PointId, distribution: MultivariateNormalDistribution)

case class Profiles(private[scalismo] val data: immutable.IndexedSeq[Profile]) {
  def apply(profileId: ProfileId): Profile = data(profileId.id)
  def ids: IndexedSeq[ProfileId] = data.indices.map(idx => ProfileId(idx))
}

/**
 * Class of Discrete Fields where to each point, a DenseVector of arbitrary dimensionality is associated.
 *
 * An example instance of such a class is the set of profile points and associated features in an Active Shape Model.
 *
 */

class DiscreteFeatureField[D <: Dim: NDSpace](domain: DiscreteDomain[D], _values: IndexedSeq[DenseVector[Double]])
    extends DiscreteField[D, DenseVector[Double]](domain, _values) {

  override def apply(id: PointId) = _values(id.id)

  override def isDefinedAt(id: PointId) = id.id < domain.numberOfPoints

  override def values = _values.toIterator

  override def interpolateNearestNeighbor(): Field[D, DenseVector[Double]] = {
    Field(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }
}

object DiscreteFeatureField {
  def apply[D <: Dim: NDSpace](domain: DiscreteDomain[D], values: IndexedSeq[DenseVector[Double]]) = new DiscreteFeatureField[D](domain, values)
}