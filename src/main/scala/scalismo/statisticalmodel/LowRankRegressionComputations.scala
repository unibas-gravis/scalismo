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
package scalismo.statisticalmodel

import breeze.linalg.{Axis, DenseMatrix, DenseVector}
import scalismo.common.{DiscreteDomain, DiscreteField, PointId, Vectorizer}
import scalismo.geometry.{NDSpace, Point}
import scalismo.statisticalmodel.LowRankGaussianProcess.Eigenpair

/** This class and its companion objects are helper, to factor the computation is the
 * lowrank gaussian process regression, that are used at many places in the code.
 * The notation is based on
 * Albrecht, Thomas, et al. "Posterior shape models." Medical image analysis 17.8 (2013): 959-973.
 * See this paper for an explanation of what the individual terms do
 *
 **/
private[scalismo] case class LowRankRegressionComputation(Minv: DenseMatrix[Double],
                                                          yVec: DenseVector[Double],
                                                          meanVec: DenseVector[Double],
                                                          QtL: DenseMatrix[Double])
private[scalismo] object LowRankRegressionComputation {

  /**
   * perform the generic regression computations given a low rank gaussian process
   */
  def fromLowrankGP[D: NDSpace, Dom[D] <: DiscreteDomain[D], Value](
    gp: LowRankGaussianProcess[D, Value],
    trainingData: IndexedSeq[(Point[D], Value, MultivariateNormalDistribution)],
    naNStrategy: NaNStrategy
  )(implicit vectorizer: Vectorizer[Value]): LowRankRegressionComputation = {

    val outputDim = gp.outputDim

    val (xs, ys, errorDistributions) = trainingData.unzip3

    val yVec = DiscreteField.vectorize[D, Value](ys)
    val meanValues = xs.map(gp.mean)
    val mVec = DiscreteField.vectorize[D, Value](meanValues)

    val Q = DenseMatrix.zeros[Double](trainingData.size * outputDim, gp.klBasis.size)
    for ((x_i, i) <- xs.zipWithIndex; (Eigenpair(lambda_j, phi_j), j) <- gp.klBasis.zipWithIndex) {
      // TODO: check if not too slow
      Q(i * outputDim until i * outputDim + outputDim, j) := vectorizer.vectorize(phi_j(x_i)) * math.sqrt(lambda_j)
    }

    doComputation(yVec, mVec, Q, outputDim, errorDistributions, naNStrategy)
  }

  /**
   * perform the generic regression computations given a discrete low rank gaussian process
   */
  def fromDiscreteLowRankGP[D: NDSpace, Dom[D] <: DiscreteDomain[D], Value](
    gp: DiscreteLowRankGaussianProcess[D, Dom, Value],
    trainingData: IndexedSeq[(PointId, Value, MultivariateNormalDistribution)],
    naNStrategy: NaNStrategy
  )(implicit vectorizer: Vectorizer[Value]): LowRankRegressionComputation = {
    val outputDim = gp.outputDim
    val (ptIds, ys, errorDistributions) = trainingData.unzip3

    val yVec = DiscreteField.vectorize[D, Value](ys)

    val meanValues = DenseVector(ptIds.toArray.flatMap { ptId =>
      gp.meanVector(ptId.id * outputDim until (ptId.id + 1) * outputDim).toArray
    })

    val Q = DenseMatrix.zeros[Double](trainingData.size * outputDim, gp.rank)
    for ((ptId, i) <- ptIds.zipWithIndex; j <- 0 until gp.rank) {
      val eigenVecAtPoint = gp.basisMatrix((ptId.id * outputDim) until ((ptId.id + 1) * outputDim), j).map(_.toDouble)
      Q(i * outputDim until i * outputDim + outputDim, j) := eigenVecAtPoint * math.sqrt(gp.variance(j))
    }

    doComputation(yVec, meanValues, Q, outputDim, errorDistributions, naNStrategy)
  }

  /**
   * performs the actual computations, as described in the aformentioned paper.
   * The computation removes all rows of the mean and Q matrix, whose corresponding observation
   * in yVec contains has a value NaN.
   */
  private def doComputation(yVec: DenseVector[Double],
                            meanVec: DenseVector[Double],
                            Q: DenseMatrix[Double],
                            outputDim: Int,
                            errorDistributions: Seq[MultivariateNormalDistribution],
                            naNStrategy: NaNStrategy): LowRankRegressionComputation = {
    // What we are actually computing here is the following:
    // L would be a block diagonal matrix, which contains on the diagonal the blocks that describes the uncertainty
    // for each point (a d x d) block. We then would compute Q.t * L. For efficiency reasons (L could be large but is sparse)
    // we avoid ever constructing the matrix L and do the multiplication by hand.
    val QtL = Q.t.copy
    assert(QtL.cols == errorDistributions.size * outputDim)
    for ((errDist, i) <- errorDistributions.zipWithIndex) {
      QtL(::, i * outputDim until (i + 1) * outputDim) := QtL(::, i * outputDim until (i + 1) * outputDim) * breeze.linalg
        .inv(errDist.cov)
    }

    // If NanStrategy is set to allow for missing values, we treat the NaNs in the vector yVec as missing observations.
    // We filter out the entries corresponding to these missing
    // observations in all the relevant vectors and then do the computations with the
    // reduced vectors. If all elements of an observation are NaN, this would then be the same
    // as if the observation would not have been included in the training data.
    val (missingEntries, entriesWithObservation) = naNStrategy match {
      case NaNStrategy.NaNAsMissingValue => (0 until yVec.length).partition(i => yVec(i).isNaN)
      case NaNStrategy.NanIsNumericValue =>
        (Seq[Int](), (0 until yVec.length)) // all values are treated as normal values
    }
    val yVecObserved = yVec(entriesWithObservation).toDenseVector
    val mVecObserved = meanVec(entriesWithObservation).toDenseVector

    val QtLObserved = QtL.delete(missingEntries, Axis._1)

    val QObserved = Q.delete(missingEntries, Axis._0)

    val M = QtLObserved * QObserved + DenseMatrix.eye[Double](Q.cols)

    val Minv = breeze.linalg.pinv(M)
    LowRankRegressionComputation(Minv, yVecObserved, mVecObserved, QtLObserved)
  }

}
