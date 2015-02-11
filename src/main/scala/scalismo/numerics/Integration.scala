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
package scalismo.numerics

import breeze.linalg.DenseVector
import scalismo.common.VectorField
import scalismo.image.ScalarImage
import scalismo.geometry._


case class Integrator[D <: Dim: NDSpace](sampler: Sampler[D]) {

  def integrateScalar(img: ScalarImage[D]): Float = {
    integrateScalar(img.liftValues)
  }

  def integrateScalar(f: Function1[Point[D], Option[Float]]): Float = {
    val samples = sampler.sample
    val sum = samples.par.map { case (pt, p) => f(pt).getOrElse(0f) * (1f / p.toFloat)}.sum
    sum / samples.size
  }

  def integrateVector[DO <: Dim : NDSpace](img: VectorField[D, DO]): Vector[DO] = {
    integrateVector(img.liftValues)
  }

  def integrateVector[DO <: Dim : NDSpace](f: Function1[Point[D], Option[Vector[DO]]]): Vector[DO] = {
    val samples = sampler.sample

    val zeroVector = Vector.zeros[DO]
    val sum = samples.par.map { case (pt, p) => f(pt).getOrElse(zeroVector) * (1f / p.toFloat) }.foldLeft(zeroVector)((a, b) => { a + b })
    sum * (1f / (sampler.numberOfPoints - 1).toFloat)
  }

  def integrateVector(f: Function1[Point[D], Option[DenseVector[Float]]], dimensionality: Int): DenseVector[Float] = {
    val samples = sampler.sample

    val zeroVector = DenseVector.zeros[Float](dimensionality)
    val sum = samples.par.map { case (pt, p) => f(pt).getOrElse(zeroVector) * (1f / p.toFloat) }.foldLeft(zeroVector)((a, b) => { a + b })
    sum * (1f / (sampler.numberOfPoints - 1).toFloat)
  }

}


