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
package scalismo.utils

import java.io.PrintStream

/**
 * Utilility class to quickly determine how long certain operations take. Example: val i = calculate(something) becomes
 * val i = benchmark(calculate(something))
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
  def benchmark[R](block: => R, desc: String = "duration", out: PrintStream = System.out): R =
    Benchmark.benchmark(block, desc, out)
}
