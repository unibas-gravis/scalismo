package org.statismo.stk.core.utils

import java.util.Map.Entry

class Memoize[-T, +R](f: T => R, cacheSizeHint: Int) extends (T => R) {

  private class Holder[X] {
    private var data: Option[X] = None

    def getOrPut(f: => X): X = {
      data match {
        case Some(v) => v
        case None => this.synchronized {
          data match {
            case Some(v) => v
            case None =>
              data = Some(f)
              data.get
          }
        }
      }
    }
  }

  private[this] val cache = new java.util.LinkedHashMap[T, Holder[R]](64, 0.75f, false) {
    override def removeEldestEntry(eldest: Entry[T, Holder[R]]) = size() > cacheSizeHint
  }

  override def apply(x: T) = {
    val holder: Holder[R] = {
      cache.get(x) match {
        case h: Holder[R] => h
        case null => cache.synchronized {
          cache.get(x) match {
            case h: Holder[R] => h
            case null =>
              val h = new Holder[R]
              cache.put(x, h)
              h
          }
        }
      }
    }
    holder.getOrPut(f(x))
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
