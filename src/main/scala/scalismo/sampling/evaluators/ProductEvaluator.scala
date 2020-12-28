/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.sampling.evaluators

import cats.kernel.Semigroup
import scalismo.sampling.{DistributionEvaluator, GradientEvaluator}

import scala.language.implicitConversions

/**
 * evaluate a product of distributions
 *
 * @param evaluators Sequence of distributions to evaluate
 */
class ProductEvaluator[A](evaluators: Seq[DistributionEvaluator[A]]) extends DistributionEvaluator[A] {
  override def logValue(sample: A): Double = {
    evaluators.iterator.map(e => e.logValue(sample)).sum
  }
}

/**
 * Evaluate a product of distribution, which have also an associated gradient
 * @param evaluators Sequence of distributions to evaluate
 */
class ProductEvaluatorWithGradient[A](evaluators: Seq[DistributionEvaluator[A] with GradientEvaluator[A]])(
  implicit semigroup: Semigroup[A]
) extends ProductEvaluator[A](evaluators)
    with GradientEvaluator[A] {

  /** gradient at sample */
  override def gradient(sample: A): A = {
    val gradients = evaluators.map(_.gradient(sample))

    // we combine (sum up) all the gradients
    gradients.reduce((g1, g2) => semigroup.combine(g1, g2))
  }
}

object ProductEvaluator {
  def apply[A](evaluators: DistributionEvaluator[A]*) = new ProductEvaluator[A](evaluators.toSeq)

  def apply[A](builder: implicits.ProductBuilder[A]) = builder.toProductEvaluator

  /**
   * Create a product of evaluators, for which it is possible to evaluate the gradient.
   * The sample (which are of type A) need to implement the semigroup typeclass, such that we
   * can combine the gradients (which are of the same type A as the sample). In the simplest and most
   * common case, the combine operation is just the sum of the parameter vectors
   */
  def withGradient[A](
    evaluators: (DistributionEvaluator[A] with GradientEvaluator[A])*
  )(implicit semigroup: Semigroup[A]): DistributionEvaluator[A] with GradientEvaluator[A] = {
    new ProductEvaluatorWithGradient[A](evaluators)
  }

  /** implicit builder for ProductEvaluator */
  object implicits {
    implicit def toProductBuilder[A](eval: DistributionEvaluator[A]): ProductBuilder[A] = new ProductBuilder[A](eval)

    implicit def toProductEvaluator[A](builder: ProductBuilder[A]): ProductEvaluator[A] = builder.toProductEvaluator

    class ProductBuilder[A](evals: DistributionEvaluator[A]*) {
      def toProductEvaluator: ProductEvaluator[A] = new ProductEvaluator[A](evals.toSeq)

      def *(other: DistributionEvaluator[A]): ProductBuilder[A] = new ProductBuilder[A](evals :+ other: _*)
    }

  }

}
