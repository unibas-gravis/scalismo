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

import scalismo.ScalismoTestSuite

class MemoizationTests extends ScalismoTestSuite {
  describe("a Function ") {

    def testFun(x: Double) = {
      Math.sin(x)
    }

    it("yields the same results after memoizing") {
      val testFunMemoized = Memoize(testFun, 100000)
      for (x <- BigDecimal(-2.0 * Math.PI) until (2.0 * Math.PI) by 0.1)
        testFun(x.doubleValue) should be(testFunMemoized(x.doubleValue))
    }

    it("evaluates faster after memoization") {
      def time[A](a: () => A): Double = {
        val now = System.nanoTime
        a()
        System.nanoTime() - now
      }

      def slowTestFun(x: Double) = {
        Thread.sleep(100)
        testFun(x)
      }

      val slowFunMemoized = Memoize(slowTestFun, 10)
      val timeSlowFun = time { () =>
        for (i <- 0 until 10) slowTestFun(0)
      }
      val timeMemoFun = time { () =>
        for (i <- 0 until 10) slowFunMemoized
      }
      timeSlowFun should be > timeMemoFun
    }
  }

}
