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

import scalismo.sampling.DistributionEvaluator

import scala.language.implicitConversions

/**
 * evaluate a product of distributions
 *
 * @param evaluators
 *   Sequence of distributions to evaluate
 */
class ProductEvaluator[A](evaluators: Seq[DistributionEvaluator[A]]) extends DistributionEvaluator[A] {
  override def logValue(sample: A): Double = {
    evaluators.iterator.map(e => e.logValue(sample)).sum
  }
}

object ProductEvaluator {
  def apply[A](evaluators: DistributionEvaluator[A]*) = new ProductEvaluator[A](evaluators.toSeq)

  def apply[A](builder: implicits.ProductBuilder[A]) = builder.toProductEvaluator

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
