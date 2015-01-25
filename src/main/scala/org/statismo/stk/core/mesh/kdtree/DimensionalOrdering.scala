/*
*
* This file is shamelessly copied from 
* https://github.com/thesamet/kdtree-scala
*/

package org.statismo.stk.core.mesh.kdtree

import scala.annotation.tailrec
import org.statismo.stk.core.geometry._


/**
 * DimensionalOrdering is a trait whose instances each represent a strategy for ordering instances
 * of a multidimensional type by a projection on a given dimension.
 */
private [core] trait DimensionalOrdering[A] {
  /** How many dimensions type A has. */
  def dimensions: Int

  /**
   * Returns an integer whose sign communicates how x's projection on a given dimension compares
   * to y's.
   *
   * Denote the projection of x and y on `dimension` by x' and y' respectively. The result sign has
   * the following meaning:
   *
   * - negative if x' < y'
   * - positive if x' > y'
   * - zero if x' == y'
   */
  def compareProjection(dimension: Int)(x: A, y: A): Int

  /**
   * Returns an Ordering of A in which the given dimension is the primary ordering criteria.
   * If x and y have the same projection on that dimension, then they are compared on the lowest
   * dimension that is different.
   */
  def orderingBy(dimension: Int): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = {
      @tailrec
      def compare0(cd: Int): Int = {
        if (cd == dimensions) 0
        else {
          val c = compareProjection(cd)(x, y)
          if (c != 0) c
          else compare0(cd + 1)
        }
      }

      compareProjection(dimension)(x, y) match {
        case t if t != 0 => t
        case 0 => compare0(0)
      }
    }
  }
}

private [core] object DimensionalOrdering {
  def dimensionalOrderingForTuple[T <: Product, A](dim: Int)(implicit ord: Ordering[A]) =
    new DimensionalOrdering[T] {
      val dimensions = dim
      def compareProjection(d: Int)(x: T, y: T) = ord.compare(
        x.productElement(d).asInstanceOf[A], y.productElement(d).asInstanceOf[A])
    }

  implicit def dimensionalOrderingForPoint[D <: Dim : NDSpace] = new DimensionalOrdering[Point[D]] {
    val dimensions = implicitly[NDSpace[D]].dimensionality
    def compareProjection(d: Int)(x: Point[D], y: Point[D]) =
      Ordering[Double].compare(x(d), y(d))
  }

  //}

  implicit def dimensionalOrderingForTuple2[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A), A](2)

  implicit def dimensionalOrderingForTuple3[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A, A), A](3)

  implicit def dimensionalOrderingForTuple4[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A, A, A), A](4)

  implicit def dimensionalOrderingForTuple5[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A, A, A, A), A](5)

}
