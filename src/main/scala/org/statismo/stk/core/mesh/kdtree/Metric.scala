/*
*
* This file is shamelessly copied from 
* https://github.com/thesamet/kdtree-scala
*/

package org.statismo.stk.core.mesh.kdtree

import scala.math.Numeric.Implicits._
import org.statismo.stk.core.geometry._

/**
 * Metric is a trait whose instances each represent a way to measure distances between
 * instances of a type.
 *
 * `A` represents the type of the points and `R` represents the metric value.
 */
trait Metric[A, R] {
  /** Returns the distance between two points. */
  def distance(x: A, y: A): R

  /**
   * Returns the distance between x and a hyperplane that passes through y and perpendicular to
   * that dimension.
   */
  def planarDistance(dimension: Int)(x: A, y: A): R
}

object Metric {
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

  implicit def metricFromCoordVectorD[D <: Dim: ToInt](implicit n: Numeric[Double]) = new Metric[Point[D], Double] {
    val dim = implicitly[ToInt[D]].toInt
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
