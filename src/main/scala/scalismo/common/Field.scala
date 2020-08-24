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
import scalismo.transformations.{CanDifferentiateWRTPosition, Transformation}

import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag

/**
 * Utility functions to create and manipulate images
 */
object Field {

  def apply[D, A](dom: Domain[D], fun: Point[D] => A) = new Field[D, A] {
    override val domain = dom
    override val f = fun
  }

  /**
   * Lifts a function between pixel values such that it acts on image intensities.
   * This is useful to write functions that manipulate the image intensities.
   */
  def lift[D, A](fl: A => A): Field[D, A] => Field[D, A] = { field: Field[D, A] =>
    new Field[D, A] {
      override val domain = field.domain
      override val f = field.f
      override def apply(x: Point[D]) = fl(field.apply(x))
    }
  }

}

object Field1D {
  def apply[A](dom: Domain[_1D], fun: Point[_1D] => A): Field[_1D, A] = Field(dom, fun)
}

object Field2D {
  def apply[A](dom: Domain[_2D], fun: Point[_2D] => A): Field[_2D, A] = Field(dom, fun)
}

object Field3D {
  def apply[A](dom: Domain[_3D], fun: Point[_3D] => A): Field[_3D, A] = Field(dom, fun)
}

/**
 * An image is simply a function from points to values, together with a domain on which the
 * function is defined.
 */
trait Field[D, A] extends Function1[Point[D], A] {
  self =>

  def domain: Domain[D]
  def f: Point[D] => A

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

  def compose(t: Point[D] => Point[D]): Field[D, A] = {
    val f = this.f.compose(t)
    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => isDefinedAt(t(pt)))
    Field[D, A](newDomain, f)
  }

  override def andThen[B](g: A => B): Field[D, B] = {
    Field(domain, f.andThen(g))
  }

  def +(that: Field[D, A])(implicit scalar: Scalar[A]): Field[D, A] = {
    val newFun = (x: Point[D]) => scalar.plus(this.f(x), that.f(x))
    val newDomain = Domain.intersection(this.domain, that.domain)
    Field(newDomain, newFun)
  }

  def -(that: Field[D, A])(implicit scalar: Scalar[A]): Field[D, A] = {
    val newFun = (x: Point[D]) => scalar.minus(this.f(x), that.f(x))
    val newDomain = Domain.intersection(this.domain, that.domain)
    Field(newDomain, newFun)
  }

  def :*(that: Field[D, A])(implicit scalar: Scalar[A]): Field[D, A] = {
    val newFun = (x: Point[D]) => scalar.times(this.f(x), that.f(x))
    val newDomain = Domain.intersection(this.domain, that.domain)
    Field(newDomain, newFun)
  }

  def *(d: Double)(implicit scalar: Scalar[A]): Field[D, Double] = {
    val newFun = (x: Point[D]) => scalar.timesDouble(this.f(x), d)
    Field(this.domain, newFun)
  }

  /**
   * Lifts the definition of the value function such that it is defined everywhere,
   * but yields none if the value is outside of the domain
   */
  def liftValues: Field[D, Option[A]] = {
    val fun = { (x: Point[D]) =>
      if (self.isDefinedAt(x)) Some(self.f(x))
      else None
    }
    Field[D, Option[A]](RealSpace[D], fun)
  }

  def discretize[DDomain[DD] <: DiscreteDomain[DD]](domain: DDomain[D],
                                                    outsideValue: A): DiscreteField[D, DDomain, A] = {

    val nbChunks = Runtime.getRuntime().availableProcessors() * 2
    val pointChunks = ParVector.fromSpecific(domain.pointSet.pointsInChunks(nbChunks))

    val values =
      for {
        points <- pointChunks
        point <- points
      } yield {
        if (isDefinedAt(point)) f(point)
        else outsideValue
      }
    DiscreteField(domain, values.toIndexedSeq)
  }

}

trait DifferentiableField[D, A] extends Field[D, A] { self =>
  def df: Point[D] => EuclideanVector[D]
  def scalar: Scalar[A]

  def differentiate: Field[D, EuclideanVector[D]] = {
    Field(domain, df)
  }

  def compose(t: Transformation[D] with CanDifferentiateWRTPosition[D]): DifferentiableField[D, A] = {
    def f(x: Point[D]) = this.f(t(x))
    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => this.isDefinedAt(t(pt)))
    val df = (x: Point[D]) => {
      val dtx = t.derivativeWRTPosition(x)
      val dftx: EuclideanVector[D] = this.df(t(x))
      dtx * dftx
    }

    DifferentiableField(newDomain, f, df)(scalar)
  }

}

object DifferentiableField {
  def apply[D, A](domain: Domain[D], f: Point[D] => A, df: Point[D] => EuclideanVector[D])(
    implicit scalar: Scalar[A]
  ): DifferentiableField[D, A] = {

    val outerdf = df
    val outerf = f
    val outerdomain = domain
    val outerscalar = scalar

    new DifferentiableField[D, A] {
      override val scalar = outerscalar
      override val domain = outerdomain
      override val f = outerf
      override val df = outerdf
    }
  }
}

object DifferentiableField1D {
  def apply[A](domain: Domain[_1D], f: Point[_1D] => A, df: Point[_1D] => EuclideanVector[_1D])(
    implicit scalar: Scalar[A]
  ): DifferentiableField[_1D, A] = DifferentiableField(domain, f, df)
}

object DifferentiableField2D {
  def apply[A](domain: Domain[_2D], f: Point[_2D] => A, df: Point[_2D] => EuclideanVector[_2D])(
    implicit scalar: Scalar[A]
  ): DifferentiableField[_2D, A] = DifferentiableField(domain, f, df)
}

object DifferentiableField3D {
  def apply[A](domain: Domain[_3D], f: Point[_3D] => A, df: Point[_3D] => EuclideanVector[_3D])(
    implicit scalar: Scalar[A]
  ): DifferentiableField[_3D, A] = DifferentiableField(domain, f, df)
}
