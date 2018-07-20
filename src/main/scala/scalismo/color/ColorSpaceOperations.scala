/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.color

import scalismo.geometry.{ Dim, NDSpace, Vector }

import scala.annotation.tailrec

/** vector space operations for a pixel type, necessary for filtering */
trait ColorSpaceOperations[@specialized(Double, Float) A] {
  /** blending using vector space operations */
  def blend(v: A, w: A, l: Double): A = add(scale(v, l), scale(w, 1.0 - l))

  /** add two pixels */
  def add(pix1: A, pix2: A): A

  /** scalar multiplication */
  def scale(pix: A, l: Double): A

  /** dot product */
  def dot(pix1: A, pix2: A): Double

  /** channel-wise multiplication */
  def multiply(pix1: A, pix2: A): A

  /** zero element */
  def zero: A

  /** check if zero */
  def isZero(pix: A): Boolean = zero == pix

  /** squared norm, derived from dot product */
  def normSq(pix: A): Double = dot(pix, pix)

  /** dimensionality of underlying vector space */
  def dimensionality: Int

  /** linear combination of vectors */
  @tailrec
  final def linearCombination(first: (A, Double), rest: (A, Double)*): A = {
    val (v, f: Double) = first
    if (rest.nonEmpty) {
      val (v1, f1: Double) = rest.head
      val combined = add(scale(v, f), scale(v1, f1))
      linearCombination((combined, 1.0), rest.tail: _*) // Seq to varargs: _*
    } else {
      scale(v, f)
    }
  }
}

object ColorSpaceOperations {

  /** implicit implementation of ColorSpaceOperations for plain Float */
  implicit val floatColorSpace: ColorSpaceOperations[Float] = new ColorSpaceOperations[Float] {
    override def add(pix1: Float, pix2: Float): Float = pix1 + pix2
    override def multiply(pix1: Float, pix2: Float): Float = pix1 * pix2
    override def dot(pix1: Float, pix2: Float): Double = pix1 * pix2
    override def scale(pix: Float, l: Double): Float = (pix * l).toFloat
    override val zero: Float = 0f
    override val dimensionality = 1
  }

  /** implicit implementation of ColorSpaceOperations for plain Double */
  implicit val doubleColorSpace: ColorSpaceOperations[Double] = new ColorSpaceOperations[Double] {
    override def add(pix1: Double, pix2: Double): Double = pix1 + pix2
    override def multiply(pix1: Double, pix2: Double): Double = pix1 * pix2
    override def dot(pix1: Double, pix2: Double): Double = pix1 * pix2
    override def scale(pix: Double, l: Double): Double = pix * l
    override val zero: Double = 0.0
    override val dimensionality = 1
  }

  /** implementation for vectors of arbitrary dimension */
  implicit def vecColorSpaceND[D <: Dim: NDSpace]: ColorSpaceOperations[Vector[D]] = new ColorSpaceOperations[Vector[D]] {
    override def add(pix1: Vector[D], pix2: Vector[D]): Vector[D] = pix1 + pix2
    override def multiply(pix1: Vector[D], pix2: Vector[D]): Vector[D] = pix1 :* pix2
    override def dot(pix1: Vector[D], pix2: Vector[D]): Double = pix1 dot pix2
    override def scale(pix: Vector[D], l: Double): Vector[D] = pix * l
    override val zero: Vector[D] = Vector.zeros[D]
    override val dimensionality: Int = NDSpace[D].dimensionality
  }

  /** implementation for type A wrapped in Option[A] */
  implicit def optionSpace[A](implicit ops: ColorSpaceOperations[A]): ColorSpaceOperations[Option[A]] = new ColorSpaceOperations[Option[A]] {
    override def add(pix1: Option[A], pix2: Option[A]): Option[A] = for (p1 <- pix1; p2 <- pix2) yield ops.add(p1, p2)
    override def multiply(pix1: Option[A], pix2: Option[A]): Option[A] = for (p1 <- pix1; p2 <- pix2) yield ops.multiply(p1, p2)
    override def dot(pix1: Option[A], pix2: Option[A]): Double = (for (p1 <- pix1; p2 <- pix2) yield ops.dot(p1, p2)).getOrElse(0.0)
    override def scale(pix: Option[A], l: Double): Option[A] = for (p1 <- pix) yield ops.scale(p1, l)
    override val zero: Option[A] = Some(ops.zero)
    override val dimensionality: Int = ops.dimensionality
  }

  /** implicit conversions to work with infix operator notations for ColorSpaceOperations[A] */
  object implicits {
    import scala.language.implicitConversions

    implicit def toVector[A](color: A)(implicit space: ColorSpaceOperations[A]): ColorSpaceVector[A] = new ColorSpaceVector[A](color)

    implicit def toColor[A](vector: ColorSpaceVector[A]): A = vector.color

    class ColorSpaceVector[A](val color: A)(implicit space: ColorSpaceOperations[A]) {
      val dimensionality: Int = space.dimensionality

      def +(other: A): A = space.add(color, other)

      def -(other: A): A = space.add(color, space.scale(other, -1.0))

      def *(factor: Double): A = space.scale(color, factor)

      def /(factor: Double): A = space.scale(color, 1.0 / factor)

      def *:(factor: Double): A = space.scale(color, factor)

      def dot(other: A): Double = space.dot(color, other)

      def multiply(other: A): A = space.multiply(color, other)

      def x(other: A): A = space.multiply(color, other)

      def normSq: Double = space.normSq(color)

      def unary_- : A = space.scale(color, -1.0)

      def isZero: Boolean = space.isZero(color)
    }
  }
}