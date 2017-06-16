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

import breeze.stats.distributions.{ Gaussian, Uniform }
import scalismo.ScalismoTestSuite

class RandomTests extends ScalismoTestSuite {

  describe("A random source") {

    it("should yield a deterministic sequence when correctly seeded") {

      def randomNumbersSeeded() = {
        val r = Random(42)
        val standardNormal = Gaussian(0, 1)(r.breezeRandBasis)
        val uniform = Uniform(0, 1)(r.breezeRandBasis)
        (r.scalaRandom.nextInt, standardNormal.draw(), uniform.draw())
      }
      randomNumbersSeeded() should equal(randomNumbersSeeded())

    }

    it("should yield a random sequence when the predefined implicit is imported") {

      def randomNumbersNotSeeded() = {
        import scalismo.utils.Random.implicits._
        val r = implicitly[Random]
        val standardNormal = Gaussian(0, 1)(r.breezeRandBasis)
        val uniform = Uniform(0, 1)(r.breezeRandBasis)
        (r.scalaRandom.nextInt, standardNormal.draw(), uniform.draw())
      }
      randomNumbersNotSeeded() should not equal (randomNumbersNotSeeded())
    }
  }
}
