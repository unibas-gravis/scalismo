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
import scalismo.common.{Scalar, VectorField}
import scalismo.geometry._
import scalismo.image.ScalarImage

case class Integrator[D: NDSpace](sampler: Sampler[D]) {

  def integrateScalar[A: Scalar](img: ScalarImage[D, A]): A = {
    integrateScalar(img.liftValues)
  }

  def integrateScalar[A: Scalar](f: Function1[Point[D], Option[A]]): A = {
    val scalar = Scalar[A]
    val zero = scalar.fromInt(0)
    val samples = sampler.sample
    val sum = samples.par.map {
      case (pt, p) =>
        scalar.toDouble(f(pt).getOrElse(zero)) * (1.0 / p.toFloat)
    }.sum
    scalar.fromDouble(sum / samples.size)
  }

  def integrateVector[DO: NDSpace](img: VectorField[D, DO]): EuclideanVector[DO] = {
    integrateVector(img.liftValues)
  }

  def integrateVector[DO: NDSpace](f: Function1[Point[D], Option[EuclideanVector[DO]]]): EuclideanVector[DO] = {
    val samples = sampler.sample

    val zeroVector = EuclideanVector.zeros[DO]
    val sum = samples.par
      .map { case (pt, p) => f(pt).getOrElse(zeroVector) * (1.0 / p) }
      .foldLeft(zeroVector)((a, b) => { a + b })
    sum * (1f / (sampler.numberOfPoints - 1))
  }

  def integrateVector(f: Function1[Point[D], Option[DenseVector[Double]]], dimensionality: Int): DenseVector[Double] = {
    val samples = sampler.sample

    val zeroVector = DenseVector.zeros[Double](dimensionality)
    val sum = samples.par
      .map { case (pt, p) => f(pt).getOrElse(zeroVector) * (1.0 / p) }
      .foldLeft(zeroVector)((a, b) => { a + b })
    sum * (1.0 / (sampler.numberOfPoints - 1))
  }

}
