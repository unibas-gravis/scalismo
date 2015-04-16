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

import scalismo.geometry._
import spire.math.Numeric

import scala.reflect.ClassTag

/**
 * Utility functions to create and manipulate images
 */
object Field {

  def apply[D <: Dim, A](dom: Domain[D], fun: Point[D] => A) = new Field[D, A] {
    override def domain = dom
    override val f = fun
  }

  /**
   * Lifts a function between pixel values such that it acts on image intensities.
   * This is useful to write functions that manipulate the image intensities.
   */
  def lift[D <: Dim, A](fl: A => A): Field[D, A] => Field[D, A] = {
    img: Field[D, A] =>
      new Field[D, A] {
        override def apply(x: Point[D]) = fl(img.apply(x))
        override val f = img.f
        def domain = img.domain
      }
  }

}

/**
 * An image is simply a function from points to values, together with a domain on which the
 * function is defined.
 */
trait Field[D <: Dim, A] extends Function1[Point[D], A] { self =>

  /** a function that defines the image values. It must be defined on the full domain */
  protected val f: Point[D] => A

  /** The domain on which the image is defined */
  def domain: Domain[D]

  /** True if the image is defined at the given point */
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)

  /**
   * The value of the image at a given point.
   * if an image is accessed outside of its definition, an exception is thrown
   */
  override def apply(x: Point[D]): A = {
    if (!isDefinedAt(x)) throw new IllegalArgumentException(s"Point $x is outside the domain")
    f(x)
  }

  /**
   * Lifts the definition of the value function such that it is defined everywhere,
   * but yields none if the value is outside of the domain
   */
  def liftValues: (Point[D] => Option[A]) = new Field[D, Option[A]] {
    override val f = { (x: Point[D]) =>
      if (self.isDefinedAt(x)) Some(self.f(x))
      else None
    }
    override def domain = RealSpace[D]
  }

}

/**
 * A scalar valued field.
 */
case class ScalarField[D <: Dim, A: Scalar: ClassTag](domain: Domain[D], f: Point[D] => A) extends Field[D, A] {

  val ev = implicitly[Scalar[A]]
  /** adds two images. The domain of the new image is the intersection of both */
  def +(that: ScalarField[D, A]): ScalarField[D, A] = {
    def f(x: Point[D]): A = ev.fromDouble(ev.toDouble(this.f(x)) + ev.toDouble(that.f(x)))
    new ScalarField(Domain.intersection[D](domain, that.domain), f)
  }

  /** subtract two images. The domain of the new image is the intersection of the domains of the individual images*/
  def -(that: ScalarField[D, A]): ScalarField[D, A] = {
    def f(x: Point[D]): A = ev.fromDouble(ev.toDouble(this.f(x)) - ev.toDouble(that.f(x)))
    val newDomain = Domain.intersection[D](domain, that.domain)
    new ScalarField(newDomain, f)
  }

  /** scalar multiplication of a vector field */
  def *(s: Double): ScalarField[D, A] = {

    def f(x: Point[D]): A = ev.fromDouble(ev.toDouble(this.f(x)) * s)
    new ScalarField(domain, f)
  }
}

/**
 * An vector valued image.
 */
case class VectorField[D <: Dim, DO <: Dim](domain: Domain[D], f: Point[D] => Vector[DO]) extends Field[D, Vector[DO]] {

  /** adds two images. The domain of the new image is the intersection of both */
  def +(that: VectorField[D, DO]): VectorField[D, DO] = {
    def f(x: Point[D]): Vector[DO] = this.f(x) + that.f(x)
    new VectorField(Domain.intersection[D](domain, that.domain), f)
  }

  /** subtract two images. The domain of the new image is the intersection of the domains of the individual images*/
  def -(that: VectorField[D, DO]): VectorField[D, DO] = {
    def f(x: Point[D]): Vector[DO] = this.f(x) - that.f(x)
    val newDomain = Domain.intersection[D](domain, that.domain)
    new VectorField(newDomain, f)
  }

  /** scalar multiplication of a vector field */
  def *(s: Double): VectorField[D, DO] = {
    def f(x: Point[D]): Vector[DO] = this.f(x) * s.toFloat
    new VectorField(domain, f)
  }
}
