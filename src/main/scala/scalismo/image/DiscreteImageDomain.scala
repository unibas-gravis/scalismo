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
    val bb = structuredPoints.boundingBox
    BoxDomain(bb.origin, bb.oppositeCorner + spacing)
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
    val spacing = EuclideanVector1D(boundingBox.extent(0) / (size(0) + 1))
    DiscreteImageDomain(StructuredPoints1D(boundingBox.origin, spacing, size))
  }

  def apply(boundingBox: BoxDomain[_1D], spacing: EuclideanVector[_1D]): DiscreteImageDomain[_1D] = {
    val size = IntVector1D(Math.ceil(boundingBox.extent(0) / spacing(0)).toInt - 1)
    DiscreteImageDomain(StructuredPoints1D(boundingBox.origin, spacing, size))
  }
}

object DiscreteImageDomain2D {

  def apply(pointSet: StructuredPoints2D): DiscreteImageDomain[_2D] = {
    DiscreteImageDomain(pointSet)
  }

  def apply(origin: Point[_2D],
            spacing: EuclideanVector[_2D],
            size: IntVector[_2D],
            phi: Double = 0.0): DiscreteImageDomain[_2D] = {
    DiscreteImageDomain(StructuredPoints2D(origin, spacing, size, phi))
  }

  def apply(boundingBox: BoxDomain[_2D], size: IntVector[_2D]): DiscreteImageDomain[_2D] = {
    val spacing = EuclideanVector2D(boundingBox.extent(0) / (size(0) + 1), boundingBox.extent(1) / (size(1) + 1))
    DiscreteImageDomain(StructuredPoints2D(boundingBox.origin, spacing, size))
  }

  def apply(boundingBox: BoxDomain[_2D], spacing: EuclideanVector[_2D]): DiscreteImageDomain[_2D] = {
    val size = IntVector2D(Math.ceil(boundingBox.extent(0) / spacing(0)).toInt - 1,
                           Math.ceil(boundingBox.extent(1) / spacing(1)).toInt - 1)

    DiscreteImageDomain(StructuredPoints2D(boundingBox.origin, spacing, size))
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
            yaw: Double,
            pitch: Double,
            roll: Double): DiscreteImageDomain[_3D] = {
    DiscreteImageDomain(StructuredPoints3D(origin, spacing, size, yaw, pitch, roll))
  }

  def apply(boundingBox: BoxDomain[_3D], size: IntVector[_3D]): DiscreteImageDomain[_3D] = {
    val spacing = EuclideanVector3D(boundingBox.extent(0) / (size(0) + 1),
                                    boundingBox.extent(1) / (size(1) + 1),
                                    boundingBox.extent(2) / (size(2) + 1))
    DiscreteImageDomain(StructuredPoints3D(boundingBox.origin, spacing, size))
  }

  def apply(boundingBox: BoxDomain[_3D], spacing: EuclideanVector[_3D]): DiscreteImageDomain[_3D] = {
    val size = IntVector3D(
      Math.ceil(boundingBox.extent(0) / spacing(0)).toInt - 1,
      Math.ceil(boundingBox.extent(1) / spacing(1)).toInt - 1,
      Math.ceil(boundingBox.extent(2) / spacing(2)).toInt - 1
    )

    DiscreteImageDomain(StructuredPoints3D(boundingBox.origin, spacing, size))
  }
}
//
//object DiscreteImageDomain {
//  def apply[D: NDSpace: CreateStructuredPoints](origin: Point[D],
//                                                spacing: EuclideanVector[D],
//                                                size: IntVector[D]): DiscreteImageDomain[D] = {
//    new DiscreteImageDomain[D](StructuredPoints(origin, spacing, size))
//  }
//
//  /** Create structuredPoints with given bounding box */
//  def apply[D](imageBox: BoxDomain[D],
//               size: IntVector[D])(implicit evCreate: CreateStructuredPoints[D]): StructuredPoints[D] = {
//    val spacing = imageBox.extent.mapWithIndex({ case (ithExtent, i) => ithExtent / (size(i)) })
//    evCreate.create(imageBox.origin, spacing, size)
//  }
//
//  /** Create a new discreteImageDomain with given image box (i.e. a box that determines the area where the image is defined) and size */
//  def apply[D: NDSpace](imageBox: BoxDomain[D], spacing: EuclideanVector[D])(
//    implicit
//    evCreate: CreateStructuredPoints[D]
//  ): StructuredPoints[D] = {
//    val sizeFractional = imageBox.extent.mapWithIndex({ case (ithExtent, i) => ithExtent / spacing(i) })
//    val size = IntVector.apply[D](sizeFractional.toArray.map(s => Math.ceil(s).toInt))
//    evCreate.create(imageBox.origin, spacing, size)
//  }
//
//}

//
///** Typeclass for creating domains of arbitrary dimensionality */
//sealed trait CreateDiscreteImageDomain[D] {
//  def create(origin: Point[D], spacing: EuclideanVector[D], size: IntVector[D]): DiscreteImageDomain[D]
//}
//
//object CreateDiscreteImageDomain {
//  implicit object CreateImageDomain1D extends CreateDiscreteImageDomain[_1D] {
//    override def create(origin: Point[_1D],
//                        spacing: EuclideanVector[_1D],
//                        size: IntVector[_1D]): StructuredPoints[_1D] = {
//      StructuredPoints1D(origin, spacing, size)
//    }
//
//  }
//
//  implicit object CreateStructuredPoints2D extends CreateStructuredPoints[_2D] {
//    override def create(origin: Point[_2D],
//                        spacing: EuclideanVector[_2D],
//                        size: IntVector[_2D]): StructuredPoints[_2D] = {
//      new StructuredPoints2D(origin, spacing, size, 0.0)
//    }
//
//  }
//
//  implicit object CreateStructuredPoints3D extends CreateStructuredPoints[_3D] {
//    override def create(origin: Point[_3D],
//                        spacing: EuclideanVector[_3D],
//                        size: IntVector[_3D]): StructuredPoints[_3D] = {
//      new StructuredPoints3D(origin, spacing, size, 0.0, 0.0, 0.0)
//    }
//  }
