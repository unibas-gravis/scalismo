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

import breeze.stats.distributions.RandBasis

/**
 * A thin wrapper around different random number generators.
 *
 * The purpose of this class is to allow for a unified way of declaring methods that need a source
 * of randomness. The idea is that any method that needs a source of randomness takes an implicit instance to this class as an argument.
 * If the user does not specify an own implicit instance, the default source in the companion object is used.
 * If the result should be deterministic, the user can define his/her own implicit instance with a fixed seed.
 *
 * @param seed The seed for the Random number generator
 */
private[utils] case class RandomNumberGenerator(seed: Long) {

  implicit val breezeRandBasis: RandBasis = RandBasis.withSeed(seed.toInt)

  val scalaRandom: scala.util.Random = new scala.util.Random(seed)

  @deprecated("directly use breezeRandBasis and breeze.stats.distributions.Gaussian", "since v0.15")
  def breezeRandomGaussian(mu: Double, sigma: Double): breeze.stats.distributions.Rand[Double] = {
    breeze.stats.distributions.Gaussian(mu, sigma)(breezeRandBasis)
  }

  @deprecated("directly use breezeRandBasis and breeze.stats.distributions.Uniform", "since v0.15")
  def breezeRandomUniform(a: Double, b: Double): breeze.stats.distributions.Rand[Double] = {
    breeze.stats.distributions.Uniform(a, b)(breezeRandBasis)
  }

}

@scala.annotation.implicitNotFound("""missing implicit Random
To fix the missing implicit either use:
	import scalismo.utils.Random.implicits._
...or create a seeded random object:
	import scalismo.utils.Random
	implicit val rng = Random(1024L)""")
class Random()(implicit val rng: RandomNumberGenerator) {

  def scalaRandom = rng.scalaRandom

  implicit def breezeRandBasis: RandBasis = rng.breezeRandBasis

  @deprecated("directly use breezeRandBasis and breeze.stats.distributions.Gaussian", "since v0.15")
  def breezeRandomGaussian(mu: Double, sigma2: Double) = rng.breezeRandomGaussian(mu, sigma2)

  @deprecated("directly use breezeRandBasis and breeze.stats.distributions.Uniform", "since v0.15")
  def breezeRandomUnform(a: Double, b: Double) = rng.breezeRandomUniform(a, b)
}

object Random {

  def apply(seed: Long) = new Random()(RandomNumberGenerator(seed))

  object implicits {
    implicit val randomGenerator: Random = new Random()(RandomNumberGenerator(System.nanoTime()))
  }

}
