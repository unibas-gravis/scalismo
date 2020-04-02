package scalismo.common

import scalismo.geometry.{EuclideanVector, NDSpace, Point, _1D, _2D, _3D}
import scalismo.registration.Transformation

case class UnstructuredPointsDomain[D](pointSet: UnstructuredPoints[D]) extends DiscreteDomain[D] {}

object UnstructuredPointsDomain {
  def apply[D: NDSpace: Create](points: IndexedSeq[Point[D]])(implicit creator: UnstructuredPoints.Create[D]): UnstructuredPointsDomain[D] = {
    UnstructuredPointsDomain(UnstructuredPoints(points))
  }

  trait Create[D] {
    def create(points: IndexedSeq[Point[D]]): UnstructuredPointsDomain[D]
  }

  object Create {
    implicit object CreateUnstructuredPointsDomain1D extends Create[_1D] {
      override def create(points: IndexedSeq[Point[_1D]]) = new UnstructuredPointsDomain[_1D](UnstructuredPoints[_1D](points))
    }
    implicit object CreateUnstructuredPointsDomain2D extends Create[_2D] {
      override def create(points: IndexedSeq[Point[_2D]]) = new UnstructuredPointsDomain[_2D](UnstructuredPoints[_2D](points))
    }
    implicit object CreateUnstructuredPointsDomain3D extends Create[_3D] {
      override def create(points: IndexedSeq[Point[_3D]]) = new UnstructuredPointsDomain[_3D](UnstructuredPoints[_3D](points))
    }
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
