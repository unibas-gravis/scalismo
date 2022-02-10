package scalismo.common

import scalismo.geometry.EuclideanVector
import scalismo.transformations.Transformation

import scala.language.higherKinds

trait Topology[D] {}

trait DiscreteDomain[D] {
  def pointSet: PointSet[D]
}

trait DomainWarp[D, DDomain[D] <: DiscreteDomain[D]] {

  /**
   * Warp the points of the domain of the discrete field and turn it into the
   * warped domain.
   */
  def transformWithField(domain: DDomain[D], warpField: DiscreteField[D, DDomain, EuclideanVector[D]]): DDomain[D]

  def transform(domain: DDomain[D], transformation: Transformation[D]): DDomain[D]
}
