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
package scalismo

import scalismo.common.DiscreteField
import scalismo.geometry.{IntVector, NDSpace}

/**
 * Contains classes for representing discrete and continuous images as well as filters for filtering both types of images.
 *
 * In this library, the standard type of images are just functions, defined on some domain.
 * There is also the more traditional discrete image type, which represents image data on a regular pixel/voxel grid.
 *
 * DiscreteImages are mainly used for reading/writing and simple manipulation of image values. The continuous representation
 * of images is much more flexible and most method for manipulating images are defined only on this image type.
 * A discrete image can be converted to a continuous image by using an interpolation procedure:
 * {{{
 * val domain = StructuredPoints(Point(0,0), Vector(1,1), Index(255,255))
 * val di = DiscreteImage(domain)(0)
 * val discreteImage =  DiscreteImage(domain, (_ : Point[_2D]) => 1.0f)
 * val continuousImage = discreteImage.interpolate(3)
 * }}}
 *
 * To get back the discrete representation, we can sample the image values on a regular grid:
 * {{{
 *  val newDomain = StructuredPoints(Point(0,0), Vector(1,1), Index(128,128))
 *  val resampledDiscreteImage = continuousImage.sample(domain, 0)
 * }}}
 */
package object image {

  type DiscreteImage[D, A] = DiscreteField[D, DiscreteImageDomain, A]

  implicit class DiscreteImageOps[D: NDSpace, A](discreteField: DiscreteField[D, DiscreteImageDomain, A]) {

    //private val pointSet = discreteField.pointSet
    //val dimensionality = ndSpace.dimensionality

    def apply(idx: IntVector[D]): A = discreteField(discreteField.domain.pointSet.pointId(idx))

    def isDefinedAt(idx: IntVector[D]): Boolean = {
      discreteField.domain.pointSet.isDefinedAt(idx)
    }

  }
}
