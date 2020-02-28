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
package scalismo.common

import breeze.linalg.DenseVector

trait Vectorizer[Value] {
  def dim: Int

  def vectorize(v: Value): DenseVector[Double]

  def unvectorize(d: DenseVector[Double]): Value

}
object Vectorizer {
  implicit object ShortVectorizer extends Vectorizer[Short] {
    override def dim: Int = 1

    override def vectorize(v: Short): DenseVector[Double] = {
      val dv = DenseVector[Double](1)
      dv(0) = v
      dv
    }

    override def unvectorize(d: DenseVector[Double]): Short = {
      d(0).toShort
    }
  }

  implicit object IntVectorizer extends Vectorizer[Int] {
    override def dim: Int = 1

    override def vectorize(v: Int): DenseVector[Double] = {
      val dv = DenseVector[Double](1)
      dv(0) = v
      dv
    }

    override def unvectorize(d: DenseVector[Double]): Int = {
      d(0).toInt
    }
  }

  implicit object FloatVectorizer extends Vectorizer[Float] {
    override def dim: Int = 1

    override def vectorize(v: Float): DenseVector[Double] = {
      val dv = DenseVector[Double](1)
      dv(0) = v
      dv
    }

    override def unvectorize(d: DenseVector[Double]): Float = {
      d(0).toFloat
    }
  }

  implicit object DoubleVectorizer extends Vectorizer[Double] {
    override def dim: Int = 1

    override def vectorize(v: Double): DenseVector[Double] = {
      val dv = DenseVector[Double](1)
      dv(0) = v
      dv
    }

    override def unvectorize(d: DenseVector[Double]): Double = {
      d(0)
    }
  }
}
