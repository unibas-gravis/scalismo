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
import scalismo.geometry.{_1D, _2D, _3D, EuclideanVector, IntVector, NDSpace, Point}

case class DiscreteImageDomain[D: NDSpace](structuredPoints: StructuredPoints[D]) extends DiscreteDomain[D] {

  override def pointSet: StructuredPoints[D] = structuredPoints

  def origin = pointSet.origin
  def spacing = pointSet.spacing
  def size = pointSet.size

  def boundingBox: BoxDomain[D] = {

    // The image bounding box is 1*spacing larger than the bounding box of the point of the domain, as
    // every point of the domain represents one voxel.
    val bb = structuredPoints.boundingBox
    BoxDomain(bb.origin, bb.oppositeCorner + spacing)
  }
}

object DiscreteImageDomain {
  def apply[D: NDSpace: CreateStructuredPoints](origin: Point[D],
                                                spacing: EuclideanVector[D],
                                                size: IntVector[D]): DiscreteImageDomain[D] = {
    new DiscreteImageDomain[D](StructuredPoints(origin, spacing, size))
  }

  def apply[D: NDSpace: CreateStructuredPoints](boundingBox: BoxDomain[D],
                                                size: IntVector[D]): DiscreteImageDomain[D] = {
    new DiscreteImageDomain[D](StructuredPoints(boundingBox, size))
  }

  def apply[D: NDSpace: CreateStructuredPoints](boundingBox: BoxDomain[D],
                                                size: IntVector[D]): DiscreteImageDomain[D] = {
    new DiscreteImageDomain[D](StructuredPoints(boundingBox, size))
  }

}

object DiscreteImageDomain1D {
  def apply(origin: Point[_1D], spacing: EuclideanVector[_1D], size: IntVector[_1D]): DiscreteImageDomain[_1D] = {
    new DiscreteImageDomain[_1D](StructuredPoints(origin, spacing, size))
  }

  def apply[D: NDSpace: CreateStructuredPoints](boundingBox: BoxDomain[_1D],
                                                size: IntVector[_1D]): DiscreteImageDomain[_1D] = {
    new DiscreteImageDomain[_1D](StructuredPoints(boundingBox, size))
  }
}

object DiscreteImageDomain2D {
  def apply(origin: Point[_2D], spacing: EuclideanVector[_2D], size: IntVector[_2D]): DiscreteImageDomain[_2D] = {
    new DiscreteImageDomain[_2D](StructuredPoints(origin, spacing, size))
  }

  def apply[D: NDSpace: CreateStructuredPoints](boundingBox: BoxDomain[_2D],
                                                size: IntVector[_2D]): DiscreteImageDomain[_2D] = {
    new DiscreteImageDomain[_1D](StructuredPoints(boundingBox, size))
  }
}

object DiscreteImageDomain3D {
  def apply(origin: Point[_3D], spacing: EuclideanVector[_3D], size: IntVector[_3D]): DiscreteImageDomain[_3D] = {
    new DiscreteImageDomain[_3D](StructuredPoints(origin, spacing, size))
  }

  def apply[D: NDSpace: CreateStructuredPoints](boundingBox: BoxDomain[_3D],
                                                size: IntVector[_3D]): DiscreteImageDomain[_3D] = {
    new DiscreteImageDomain[_3D](StructuredPoints(boundingBox, size))
  }
}
