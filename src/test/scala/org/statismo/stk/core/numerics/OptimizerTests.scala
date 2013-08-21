package org.statismo.stk.core.numerics

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseVector
import scala.collection.mutable.Subscriber
import scala.collection.mutable.Publisher

class OptimizerTests extends FunSpec with ShouldMatchers {

  describe("The GradientDescentOptimizer") {

    val c = new CostFunction {
      override def apply(x: DenseVector[Float]): (Float, DenseVector[Float]) = {
        (x(0) * x(0), DenseVector(x(0) * 2f))
      }
      override def onlyValue(x: DenseVector[Float]) = x(0) * x(0)
    }

    it("finds the correct parameter for a simple convex function") {

      val optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.1))
      val param = optimizer.minimize(DenseVector(1f), c)
      val value = c(param)._1 
      value should be(0f plusOrMinus 1e-5f)

    }

//    it("test") {
//
//      object OptimizationProcedure extends Reactor {
//        def optimize() { 
//        val optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.1))
//        listenTo(optimizer) {
//          case IterationEvent(s, it, params, v, gradients) => println(s"value $v at iteration $it")
//        }
//        val res = optimizer.minimize(DenseVector(1.0), c)
//        res(0) should be(0.0 plusOrMinus (1e-4))
//      }
//      }
//          OptimizationProcedure.optimize()
//    }
//  }
  }
}
