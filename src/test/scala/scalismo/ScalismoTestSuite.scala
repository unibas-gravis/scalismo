package scalismo

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
class ScalismoTestSuite extends AnyFunSpec with Matchers {
  scalismo.initialize()

  def measureTime[A](func: => A): (A, Double) = {
    val startTime = System.currentTimeMillis()
    val result = func
    val endTime = System.currentTimeMillis()
    val elapsedTime = (endTime - startTime)
    (result, elapsedTime)
  }
}
