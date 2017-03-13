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

import breeze.stats.distributions.{ ThreadLocalRandomGenerator, RandBasis }
import org.apache.commons.math3.random.MersenneTwister

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
case class Random(seed: Long) {

  implicit val breezeRandBasis: RandBasis = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(seed)))

  val scalaRandom: scala.util.Random = new scala.util.Random(seed)

  def breezeRandomGaussian(mu: Double, sigma: Double): breeze.stats.distributions.Rand[Double] = {
    breeze.stats.distributions.Gaussian(mu, sigma)(breezeRandBasis)
  }

  def breezeRandomUniform(a: Double, b: Double): breeze.stats.distributions.Rand[Double] = {
    breeze.stats.distributions.Uniform(a, b)(breezeRandBasis)
  }

}

object Random {

  implicit val randomGenerator = Random(System.nanoTime())

}
