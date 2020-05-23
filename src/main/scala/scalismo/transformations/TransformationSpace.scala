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
package scalismo.transformations

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{Domain, Field}
import scalismo.geometry._
import scalismo.transformations.TransformationSpace.{ParameterVector}
import scalismo.transformations.{
  CanDifferentiateWRTPosition,
  CompositeTransformation,
  ParametricTransformation,
  Translation
}
import scalismo.utils.Memoize

import scala.annotation._

/**
 * Trait for a parametric transformations space.
 *
 *  Many pre-implemented transformations spaces implement this trait : [[TranslationSpace]], [[ScalingSpace]], [[RotationSpace]] ..
 *
 *  Most of the basic transforms in our library (scaling, translation, rotation ..) can be created directly and do not necessarily require
 *  the prior creation of a TransformationSpace.
 *  Defining a TransformationSpace is mainly useful during a registration process where one optimizes over a set of parameters to retrieve
 *  a desired transformations.
 *
 */
trait TransformationSpace[D] {

  type T[D] <: ParametricTransformation[D]

  def domain: Domain[D]

  @deprecated("please use numberOfParameters instead", "v0.19")
  def parametersDimensionality = numberOfParameters

  def numberOfParameters: Int

  def transformationForParameters(p: ParameterVector): T[D]

  /** returns identity transformation) */
  def identityTransformation: T[D]
}

trait TransformationSpaceWithDifferentiableTransforms[D] extends TransformationSpace[D] {

  type T[D] <: ParametricTransformation[D] with CanDifferentiateWRTPosition[D]

  def domain: Domain[D]

  def numberOfParameters: Int

  def transformationForParameters(p: ParameterVector): T[D]

  /** returns identity transformation) */
  def identityTransformation: T[D]
}

object TransformationSpace {

  /** Type alias for parameters used with parametric transformations spaces. Currently, these are [[breeze.linalg.DenseVector]]*/
  type ParameterVector = DenseVector[Double]
}
