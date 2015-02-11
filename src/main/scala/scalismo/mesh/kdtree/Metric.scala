/*
 * Copyright 2012 Nadav Samet  https://github.com/thesamet/kdtree-scala
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

package scalismo.mesh.kdtree

import scalismo.geometry.{Dim, NDSpace, Point}

import scala.math.Numeric.Implicits._

/**
 * Metric is a trait whose instances each represent a way to measure distances between
 * instances of a type.
 *
 * `A` represents the type of the points and `R` represents the metric value.
 */
private [scalismo] trait Metric[A, R] {
  /** Returns the distance between two points. */
  def distance(x: A, y: A): R

  /**
   * Returns the distance between x and a hyperplane that passes through y and perpendicular to
   * that dimension.
   */
  def planarDistance(dimension: Int)(x: A, y: A): R
}

private [scalismo] object Metric {
  implicit def metricFromTuple2[A](implicit n: Numeric[A]) = new Metric[(A, A), A] {
    def distance(x: (A, A), y: (A, A)): A = {
      val dx = (x._1 - y._1)
      val dy = (x._2 - y._2)
      dx * dx + dy * dy
    }

    def planarDistance(d: Int)(x: (A, A), y: (A, A)): A = {
      val dd = x.productElement(d).asInstanceOf[A] - y.productElement(d).asInstanceOf[A]
      dd * dd
    }
  }

  implicit def metricFromTuple3[A](implicit n: Numeric[A]) = new Metric[(A, A, A), A] {
    def distance(x: (A, A, A), y: (A, A, A)): A = {
      val dx = (x._1 - y._1)
      val dy = (x._2 - y._2)
      val dz = (x._3 - y._3)
      dx * dx + dy * dy + dz * dz
    }

    def planarDistance(d: Int)(x: (A, A, A), y: (A, A, A)): A = {
      val dd = x.productElement(d).asInstanceOf[A] - y.productElement(d).asInstanceOf[A]
      dd * dd
    }
  }

  implicit def metricFromCoordVectorD[D <: Dim: NDSpace](implicit n: Numeric[Double]) = new Metric[Point[D], Double] {
    val dim = implicitly[NDSpace[D]].dimensionality
    def distance(x: Point[D], y: Point[D]): Double = {
      var i = 0
      var v = 0.0
      while (i < dim) {
        val d = x(i) - y(i)
        v += d * d
        i += 1
      }
      v
    }

    def planarDistance(d: Int)(x: Point[D], y: Point[D]): Double = {

      val dd = x(d) - y(d)
      dd * dd
    }
  }

 
}
