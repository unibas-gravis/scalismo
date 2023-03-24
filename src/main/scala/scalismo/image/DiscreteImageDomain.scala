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

import scalismo.common.{BoxDomain, DiscreteDomain}
import scalismo.geometry.{
  _1D,
  _2D,
  _3D,
  EuclideanVector,
  EuclideanVector1D,
  EuclideanVector2D,
  EuclideanVector3D,
  IntVector,
  IntVector1D,
  IntVector2D,
  IntVector3D,
  NDSpace,
  Point
}

import scala.language.implicitConversions

case class DiscreteImageDomain[D: NDSpace](structuredPoints: StructuredPoints[D]) extends DiscreteDomain[D] {

  override def pointSet: StructuredPoints[D] = structuredPoints

  def origin = pointSet.origin
  def spacing = pointSet.spacing
  def size = pointSet.size

  def boundingBox: BoxDomain[D] = {

    // The image bounding box is 1*spacing larger than the bounding box of the point of the domain, as
    // every point of the domain represents one voxel.
    // (The voxel that is defined by each grid point extends by the length spacing(i) into the
    // i-th space direction).
    val bb = structuredPoints.boundingBox
    BoxDomain(bb.origin, bb.oppositeCorner + structuredPoints.directions * spacing)
  }

}

object DiscreteImageDomain1D {

  def apply(pointSet: StructuredPoints1D): DiscreteImageDomain[_1D] = {
    DiscreteImageDomain(pointSet)
  }

  def apply(origin: Point[_1D], spacing: EuclideanVector[_1D], size: IntVector[_1D]): DiscreteImageDomain[_1D] = {
    DiscreteImageDomain(StructuredPoints1D(origin, spacing, size))
  }

  def apply(boundingBox: BoxDomain[_1D], size: IntVector[_1D]): DiscreteImageDomain[_1D] = {
    DiscreteImageDomain(StructuredPoints1D(boundingBox, size))
  }

  def apply(boundingBox: BoxDomain[_1D], spacing: EuclideanVector[_1D]): DiscreteImageDomain[_1D] = {
    DiscreteImageDomain(StructuredPoints1D(boundingBox, spacing))
  }
}

object DiscreteImageDomain2D {

  def apply(pointSet: StructuredPoints2D): DiscreteImageDomain[_2D] = {
    DiscreteImageDomain(pointSet)
  }

  def apply(origin: Point[_2D],
            spacing: EuclideanVector[_2D],
            size: IntVector[_2D],
            iVec: EuclideanVector[_2D] = EuclideanVector2D(1, 0),
            jVec: EuclideanVector[_2D] = EuclideanVector2D(0, 1)
  ): DiscreteImageDomain[_2D] = {
    DiscreteImageDomain(StructuredPoints2D(origin, spacing, size, iVec, jVec))
  }

  def apply(boundingBox: BoxDomain[_2D], size: IntVector[_2D]): DiscreteImageDomain[_2D] = {
    DiscreteImageDomain(StructuredPoints2D(boundingBox, size))
  }

  def apply(boundingBox: BoxDomain[_2D], spacing: EuclideanVector[_2D]): DiscreteImageDomain[_2D] = {
    DiscreteImageDomain(StructuredPoints2D(boundingBox, spacing))
  }
}

object DiscreteImageDomain3D {

  def apply(pointSet: StructuredPoints3D): DiscreteImageDomain[_3D] = {
    DiscreteImageDomain(pointSet)
  }

  def apply(origin: Point[_3D], spacing: EuclideanVector[_3D], size: IntVector[_3D]): DiscreteImageDomain[_3D] = {
    DiscreteImageDomain(StructuredPoints3D(origin, spacing, size))
  }

  def apply(origin: Point[_3D],
            spacing: EuclideanVector[_3D],
            size: IntVector[_3D],
            iVec: EuclideanVector[_3D],
            jVec: EuclideanVector[_3D],
            kVec: EuclideanVector[_3D]
  ): DiscreteImageDomain[_3D] = {
    DiscreteImageDomain(StructuredPoints3D(origin, spacing, size, iVec, jVec, kVec))
  }

  def apply(boundingBox: BoxDomain[_3D], size: IntVector[_3D]): DiscreteImageDomain[_3D] = {
    DiscreteImageDomain(StructuredPoints3D(boundingBox, size))
  }

  def apply(boundingBox: BoxDomain[_3D], spacing: EuclideanVector[_3D]): DiscreteImageDomain[_3D] = {
    DiscreteImageDomain(StructuredPoints3D(boundingBox, spacing))
  }
}
