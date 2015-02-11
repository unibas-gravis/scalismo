package scalismo.utils

import scala.language.implicitConversions
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.matchers.ShouldMatchers

class MemoizationTests extends FunSpec with Matchers {
  describe("a Function ") {

    def testFun(x: Double) = {
      Math.sin(x)
    }

    it("yields the same results after memoizing") {
      val testFunMemoized = Memoize(testFun, 100000)
      for (x <- -2 * Math.PI until 2 * Math.PI by 0.1)
        testFun(x) should be(testFunMemoized(x))
    }

    it("evaluates faster after memoization") {
      def time[A](a: => A): Double = {
        val now = System.nanoTime
        val result = a
        System.nanoTime() - now
      }

      def slowTestFun(x: Double) = {
        Thread.sleep(100)
        testFun(x)
      }

      val slowFunMemoized = Memoize(slowTestFun, 10)
      val timeSlowFun = time { 
        for (i <- 0 until 10) slowTestFun(0)
      }
      val timeMemoFun = time { 
        for (i <- 0 until 10) slowFunMemoized
      }
      timeSlowFun should be > timeMemoFun 
    }
  }

}
