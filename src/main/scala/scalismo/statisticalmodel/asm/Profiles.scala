package scalismo.statisticalmodel.asm

import scalismo.common.{ DiscreteField, SpatiallyIndexedDiscreteDomain }
import scalismo.geometry._3D
import scalismo.statisticalmodel.MultivariateNormalDistribution

import scala.collection.immutable

case class Profiles(domain: SpatiallyIndexedDiscreteDomain[_3D], data: immutable.IndexedSeq[MultivariateNormalDistribution])
    extends DiscreteField[_3D, MultivariateNormalDistribution] {
  require(domain.numberOfPoints == data.size)

  def apply(i: Int) = data(i)

  def isDefinedAt(i: Int) = data.isDefinedAt(i)

  def values = data.iterator
}

