package scalismo.common

import scalismo.common.UnstructuredPoints.Create
import scalismo.geometry.{EuclideanVector, NDSpace, Point}
import scalismo.registration.Transformation

case class UnstructuredPointsDomain[D](pointSet: UnstructuredPoints[D]) extends DiscreteDomain[D] {}

object UnstructuredPointsDomain {
  def apply[D: NDSpace: Create](points: IndexedSeq[Point[D]]): UnstructuredPointsDomain[D] = {
    UnstructuredPointsDomain(UnstructuredPoints(points))
  }

  implicit def warper[D: NDSpace](
    implicit
    creator: UnstructuredPoints.Create[D]
  ): DomainWarp[D, UnstructuredPointsDomain] = {
    new DomainWarp[D, UnstructuredPointsDomain] {
      override def transformWithField(
        domain: UnstructuredPointsDomain[D],
        warpField: DiscreteField[D, UnstructuredPointsDomain, EuclideanVector[D]]
      ): UnstructuredPointsDomain[D] = {
        val warpedPoints = for ((p, v) <- warpField.pointsWithValues) yield {
          p + v
        }
        UnstructuredPointsDomain(UnstructuredPoints(warpedPoints.toIndexedSeq))
      }

      override def transform(domain: UnstructuredPointsDomain[D],
                             transformation: Transformation[D]): UnstructuredPointsDomain[D] = {
        UnstructuredPointsDomain(domain.pointSet.transform(transformation))
      }
    }
  }
}
