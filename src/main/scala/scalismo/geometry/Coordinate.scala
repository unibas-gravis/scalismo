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
package scalismo.geometry

import breeze.linalg.DenseVector

/**
 * The basic n-tuple in R^n^ with scalar type S
 */
abstract class Coordinate[D <: Dim: NDSpace, @specialized(Int, Float, Double) S] {
  val dimensionality: Int = implicitly[NDSpace[D]].dimensionality

  private[scalismo] val data: Array[S]

  def apply(i: Int): S = data(i)

  def toBreezeVector = DenseVector(data)

  override def hashCode = data.deep.hashCode()

  override def equals(other: Any): Boolean = other match {
    case that: Coordinate[D, S] => that.canEqual(this) && this.data.deep == that.data.deep
    case _ => false
  }

  protected def canEqual(other: Any): Boolean

  override def toString = data.deep.toString()
}

