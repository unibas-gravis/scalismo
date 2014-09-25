package org.statismo.stk.core.utils

import java.io.PrintStream

/**
 * Utilility class to quickly determine how long certain operations take.
 * Example:
 *   val i = calculate(something)
 *   becomes
 *   val i = benchmark(calculate(something))
 *
 * Available to call statically (Benchmark.benchmark) or as a mixin trait, for convenience.
 */

object Benchmark {
  def benchmark[R](block: => R, desc: String = "duration", out: PrintStream = System.out): R = {
    val start = System.currentTimeMillis()
    val result = block
    out.println(desc + ": " + (System.currentTimeMillis() - start) + " ms")
    result
  }
}

trait Benchmark {
  def benchmark[R](block: => R, desc: String = "duration", out: PrintStream = System.out): R = Benchmark.benchmark(block, desc, out)
}