package scalismo.statisticalmodel.asm

import scalismo.common.{ DiscreteField, Field, SpatiallyIndexedDiscreteDomain }
import scalismo.geometry.{ Point, _3D }
import scalismo.statisticalmodel.MultivariateNormalDistribution

import scala.collection.immutable

case class Profiles(domain: SpatiallyIndexedDiscreteDomain[_3D], data: immutable.IndexedSeq[MultivariateNormalDistribution])
    extends DiscreteField[_3D, MultivariateNormalDistribution] {
  require(domain.numberOfPoints == data.size)

  override def apply(i: Int) = data(i)

  override def isDefinedAt(i: Int) = data.isDefinedAt(i)

  override def values = data.iterator

  override def interpolateNearestNeighbor(): Field[_3D, MultivariateNormalDistribution] = {
    val indexedDomain = SpatiallyIndexedDiscreteDomain(domain.points.toIndexedSeq, domain.numberOfPoints)
    Field(domain.boundingBox, (p: Point[_3D]) => apply(indexedDomain.findClosestPoint(p)._2))
  }

}

