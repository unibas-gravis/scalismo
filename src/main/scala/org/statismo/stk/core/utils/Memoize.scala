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
}