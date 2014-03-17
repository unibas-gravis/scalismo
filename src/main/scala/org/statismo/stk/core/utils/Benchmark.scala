package org.statismo.stk.core.utils

class Benchmark {
  def benchmark[R](block: => R, desc: String = "duration"): R = {
    val start = System.currentTimeMillis()
    val result = block
    println(desc + ": " + (System.currentTimeMillis() - start) + " ms")
    result
  }
}