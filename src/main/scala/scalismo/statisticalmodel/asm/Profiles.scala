package scalismo.statisticalmodel.asm

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry.{ NDSpace, Dim, Point, _3D }
import scalismo.statisticalmodel.MultivariateNormalDistribution

import scala.collection.immutable

case class Profiles(domain: SpatiallyIndexedDiscreteDomain[_3D], data: immutable.IndexedSeq[MultivariateNormalDistribution])
    extends DiscreteField[_3D, MultivariateNormalDistribution] {
  require(domain.numberOfPoints == data.size)

  override def apply(i: Int) = data(i)

  override def isDefinedAt(i: Int) = data.isDefinedAt(i)

  override def values = data.iterator

  override def interpolateNearestNeighbor(): Field[_3D, MultivariateNormalDistribution] = {
    Field(domain.boundingBox, (p: Point[_3D]) => apply(domain.findClosestPoint(p)._2))
  }

}

/**
 * Class of Discrete Fields where to each point, a DenseVector of arbitrary dimensionality is associated.
 *
 * An example instance of such a class is the set of profile points and associated features in an Active Shape Model.
 *
 */

case class DiscreteFeatureField[D <: Dim: NDSpace](private val pointsAndValues: IndexedSeq[(Point[D], DenseVector[Float])]) extends DiscreteField[D, DenseVector[Float]] {

  override def apply(i: Int) = pointsAndValues(i)._2

  override def isDefinedAt(i: Int) = i >= 0 && i < pointsAndValues.size

  override def domain = DiscreteDomain.fromSeq[D](pointsAndValues.map(_._1))

  override def values = pointsAndValues.map(_._2).toIterator

  override def interpolateNearestNeighbor(): Field[D, DenseVector[Float]] = {
    val indexedDom = SpatiallyIndexedDiscreteDomain.fromSeq[D](pointsAndValues.map(_._1))
    Field(RealSpace[D], (p: Point[D]) => apply(indexedDom.findClosestPoint(p)._2))
  }
}

