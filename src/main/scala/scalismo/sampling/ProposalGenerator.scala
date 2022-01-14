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
package scalismo.sampling

import breeze.linalg.DenseVector
import scalismo.sampling.proposals.{GaussianRandomWalkProposal, MixtureProposal}

/** proposal distribution sampler for Metropolis-Hastings MCMC */
trait ProposalGenerator[A] {

  /** draw a sample from this proposal distribution, may depend on current state */
  def propose(current: A): A


}

/** ratio of forward and backwards proposal probability/density */
trait TransitionRatio[A] {

  /** total rate of transition from to, corrected for backwards transition (log value) */
  def logTransitionRatio(from: A, to: A): Double
}

/** symmetric transition: equal forward and backward transition probability */
trait SymmetricTransitionRatio[A] extends TransitionRatio[A] {
  override def logTransitionRatio(from: A, to: A) = 0.0
}

/** expresses transition probability between two states */
trait TransitionProbability[A] extends TransitionRatio[A] {

  /** rate of transition from to (log value) */
  def logTransitionProbability(from: A, to: A): Double

  /** transition ratio forward probability / backward probability */
  override def logTransitionRatio(from: A, to: A): Double = {
    val fw = logTransitionProbability(from, to)
    val bw = logTransitionProbability(to, from)

    if (fw.isNaN || bw.isNaN)
      throw new Exception("NaN transition Probability!")

    if (!fw.isNegInfinity || !bw.isNegInfinity)
      fw - bw
    else
      0.0
  }
}

case class MHSample[A](parameters : A, generatedBy : String)

abstract class MHProposalGenerator[A] extends ProposalGenerator[MHSample[A]] with TransitionProbability[MHSample[A]] { self =>


  def forType[T](implicit conversion: ParameterConversion[A, T]): MHProposalGenerator[T]  = {
    new MHProposalGenerator[T] {


      /** rate of transition from to (log value) */
      override def logTransitionProbability(from: MHSample[T], to: MHSample[T]): Double = {
        self.logTransitionProbability(
          from.copy(parameters = conversion.from(from.parameters)),
          to.copy(parameters = conversion.from(to.parameters)))
      }

      /** draw a sample from this proposal distribution, may depend on current state */
      override def propose(current: MHSample[T]): MHSample[T] = {
        val origProposal = self.propose(current.copy(parameters = conversion.from(current.parameters)))
        current.copy(parameters = conversion.to(origProposal.parameters), generatedBy = origProposal.generatedBy)
      }
    }
  }
}


trait ParameterConversion[A, B] extends Function1[A, B] {
  def to(a: A): B

  def from(b: B): A

  override def apply(a: A): B = to(a)
}

object ParameterConversion {
  implicit def doubleDenseVectorConversion: ParameterConversion[DenseVector[Double], Double] = new ParameterConversion[DenseVector[Double], Double] {
    override def from(a: Double): DenseVector[Double] = DenseVector(a)

    override def to(v: DenseVector[Double]): Double = v(0)
  }

  implicit def denseVectorDenseVectorConversion: ParameterConversion[DenseVector[Double], DenseVector[Double]] = new ParameterConversion[DenseVector[Double], DenseVector[Double]] {
    override def to(a: DenseVector[Double]): DenseVector[Double] = a

    override def from(v: DenseVector[Double]): DenseVector[Double] = v
  }
}