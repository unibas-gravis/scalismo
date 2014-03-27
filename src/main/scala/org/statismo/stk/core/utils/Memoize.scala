package org.statismo.stk.core.utils

import org.statismo.stk.core.common.ImmutableLRU

class Memoize[-T, +R](f: T => R, cacheSizeHint: Int) extends (T => R) {

  @volatile private[this] var valCache = ImmutableLRU[T, R](cacheSizeHint)

  override def apply(x: T) = {
    val (maybeT, _) = valCache.get(x)
    maybeT.getOrElse {
      val newValue: R = f(x)
      valCache = (valCache + (x, newValue))._2 // ignore evicted key
      newValue
    }
  }

}

object Memoize {

  def apply[T, R](f: T => R, cacheSizeHint: Int) = new Memoize[T, R](f, cacheSizeHint)

  def memfun2[T, R, F](f: F, cacheSizeHint: Int)(implicit e: Tupler[F, T => R]): F = e.untupled(new Memoize(e.tupled(f), cacheSizeHint))

}

sealed class Tupler[U, T](val tupled: U => T, val untupled: T => U)

object Tupler {

  implicit def function0[R]: Tupler[() => R, Unit => R] =
    new Tupler((f: () => R) => (_: Unit) => f(),
      (f: Unit => R) => () => f(()))

  implicit def function1[T, R]: Tupler[T => R, T => R] = new Tupler(identity, identity)

  implicit def function2[T1, T2, R]: Tupler[(T1, T2) => R, ((T1, T2)) => R] =
    new Tupler(_.tupled, Function.untupled[T1, T2, R])

}
