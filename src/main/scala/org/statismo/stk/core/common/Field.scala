package org.statismo.stk.core.common

import org.statismo.stk.core.geometry.{Vector, Point, Dim}

/**
 * Utility functions to create and manipulate images
 */
object Field {

  def apply[D <: Dim, A](dom : Domain[D], fun : Point[D] => A) = new Field[D, A] {
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
  protected[Field] val f: Point[D] => A

  /** The domain on which the image is defined */
  def domain: Domain[D]

  /** True if the image is defined at the given point */
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)

  /** The value of the image at a given point.
   * if an image is accessed outside of its definition, an exception is thrown */
  override def apply(x: Point[D]): A = {
    if (!isDefinedAt(x)) throw new Exception(s"Point $x is outside the domain")
    f(x)
  }

  /**
   * Lifts the definition of the value function such that it is defined everywhere,
   * but yields none if the value is outside of the domain
   */
  def liftValues: (Point[D] => Option[A]) = new Field[D, Option[A]] {
    override val f = { (x : Point[D]) =>
      if (self.isDefinedAt(x)) Some(self.f(x))
      else None
    }
    override def domain = RealSpace[D]
  }

}


/**
 * An vector valued image.
 */
case class VectorField[D <: Dim, DO <: Dim](domain: Domain[D], f: Point[D] => Vector[DO]) extends Field[D, Vector[DO]] {

  /** adds two images. The domain of the new image is the intersection of both */
  def +(that: VectorField[D, DO]): VectorField[D, DO] = {
    def f(x: Point[D]): Vector[DO] = this.f(x) + that.f(x)
    new VectorField(Domain.intersection[D](domain,that.domain), f)
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
