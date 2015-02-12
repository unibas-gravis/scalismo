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
package scalismo.numerics

import org.scalatest.{ Matchers, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseVector
import scala.collection.mutable.Subscriber
import scala.collection.mutable.Publisher

class OptimizerTests extends FunSpec with Matchers {

  describe("The GradientDescentOptimizer") {

    val c = new CostFunction {
      override def apply(x: DenseVector[Float]): (Float, DenseVector[Float]) = {
        (x(0) * x(0), DenseVector(x(0) * 2f))
      }
      override def onlyValue(x: DenseVector[Float]) = x(0) * x(0)
    }

    it("finds the correct parameter for a simple convex function") {

      val optimizer = GradientDescentOptimizer(numIterations = 100, stepLength = 0.1)
      val param = optimizer.minimize(DenseVector(1f), c)
      val value = c(param)._1
      value should be(0f +- 1e-5f)

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
    //        res(0) should be(0.0 +- (1e-4))
    //      }
    //      }
    //          OptimizationProcedure.optimize()
    //    }
    //  }
  }
}
