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

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.sqrt
import scalismo.common._
import scalismo.common.interpolation.{FieldInterpolator, NearestNeighborInterpolator}
import scalismo.geometry._
import scalismo.numerics.PivotedCholesky
import scalismo.transformations.{RigidTransformation, Transformation}
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.Eigenpair
import scalismo.statisticalmodel.dataset.DataCollection
import scalismo.utils.Random

import scala.language.higherKinds

/**
 * A StatisticalMeshModel is isomorphic to a [[DiscreteLowRankGaussianProcess]]. The difference is that while the
 * DiscreteLowRankGaussianProcess models defomation fields, the StatisticalMeshModel applies the deformation fields to a
 * mesh, and warps the mesh with the deformation fields to produce a new mesh.
 *
 * @see
 *   [[DiscreteLowRankGaussianProcess]]
 */
case class PointDistributionModel[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](
  gp: DiscreteLowRankGaussianProcess[D, DDomain, EuclideanVector[D]]
)(implicit warper: DomainWarp[D, DDomain], vectorizer: Vectorizer[EuclideanVector[D]]) {

  def reference: DDomain[D] = gp.domain

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank = gp.rank

  /**
   * The mean shape
   * @see
   *   [[DiscreteLowRankGaussianProcess.mean]]
   */
  lazy val mean: DDomain[D] = warper.transformWithField(gp.domain, gp.mean)

  /**
   * The covariance between two points of the mesh with given point id.
   * @see
   *   [[DiscreteLowRankGaussianProcess.cov]]
   */
  def cov(ptId1: PointId, ptId2: PointId) = gp.cov(ptId1, ptId2)

  /**
   * draws a random shape.
   * @see
   *   [[DiscreteLowRankGaussianProcess.sample]]
   */
  def sample()(implicit rand: Random): DDomain[D] = warper.transformWithField(gp.domain, gp.sample())

  /**
   * returns the probability density for an instance of the model
   * @param instanceCoefficients
   *   coefficients of the instance in the model. For shapes in correspondence, these can be obtained using the
   *   coefficients method
   */
  def pdf(instanceCoefficients: DenseVector[Double]): Double = {
    val disVecField = gp.instance(instanceCoefficients)
    gp.pdf(disVecField)
  }

  /**
   * returns a shape that corresponds to a linear combination of the basis functions with the given coefficients c.
   * @see
   *   [[DiscreteLowRankGaussianProcess.instance]]
   */
  def instance(c: DenseVector[Double]): DDomain[D] = warper.transformWithField(gp.domain, gp.instance(c))

  /**
   * Returns a marginal StatisticalMeshModel, modelling deformations only on the chosen points of the reference
   *
   * This method proceeds by clipping the reference mesh to keep only the indicated point identifiers, and then
   * marginalizing the GP over those points. Notice that when clipping, not all indicated point ids will be part of the
   * clipped mesh, as some points may not belong to any cells anymore. Therefore 2 behaviours are supported by this
   * method :
   *
   * 1- in case some of the indicated pointIds remain after clipping and do form a mesh, a marginal model is returned
   * only for those points 2- in case none of the indicated points remain (they are not meshed), a reference mesh with
   * all indicated point Ids and no cells is constructed and a marginal over this new reference is returned
   *
   * @see
   *   [[DiscreteLowRankGaussianProcess.marginal]]
   */
  def marginal(
    ptIds: IndexedSeq[PointId]
  )(implicit creator: UnstructuredPoints.Create[D]): PointDistributionModel[D, UnstructuredPointsDomain] = {
    PointDistributionModel(gp.marginal(ptIds))
  }

  /**
   * Returns a reduced rank model, using only the leading basis functions.
   *
   * @param newRank:
   *   The rank of the new model.
   */
  def truncate(newRank: Int): PointDistributionModel[D, DDomain] = {
    PointDistributionModel(gp.truncate(newRank))
  }

  /**
   * @see
   *   [[DiscreteLowRankGaussianProcess.project]]
   */
  def project(pointData: DDomain[D]): DDomain[D] = {
    val displacements =
      reference.pointSet.points
        .zip(pointData.pointSet.points)
        .map({ case (refPt, tgtPt) => tgtPt - refPt })
        .toIndexedSeq
    val dvf =
      DiscreteField[D, DDomain, EuclideanVector[D]](reference, displacements)
    warper.transformWithField(gp.domain, gp.project(dvf))
  }

  /**
   * @see
   *   [[DiscreteLowRankGaussianProcess.coefficients]]
   */
  def coefficients(pointData: DDomain[D]): DenseVector[Double] = {
    val displacements =
      reference.pointSet.points
        .zip(pointData.pointSet.points)
        .map({ case (refPt, tgtPt) => tgtPt - refPt })
        .toIndexedSeq
    val dvf =
      DiscreteField[D, DDomain, EuclideanVector[D]](reference, displacements)
    gp.coefficients(dvf)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D])], sigma2: Double)]], but the training data
   * is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(PointId, Point[D])], sigma2: Double): PointDistributionModel[D, DDomain] = {
    val trainingDataWithDisplacements = trainingData.map { case (id, targetPoint) =>
      (id, targetPoint - reference.pointSet.point(id))
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements, sigma2)
    PointDistributionModel(posteriorGp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D], Double)]]], but the training data is defined
   * by specifying the target point instead of the displacement vector
   */
  def posterior(
    trainingData: IndexedSeq[(PointId, Point[D], MultivariateNormalDistribution)]
  ): PointDistributionModel[D, DDomain] = {
    val trainingDataWithDisplacements = trainingData.map { case (id, targetPoint, cov) =>
      (id, targetPoint - reference.pointSet.point(id), cov)
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements)
    new PointDistributionModel[D, DDomain](posteriorGp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posteriorMean(Int, Point[_3D])], sigma2: Double)]], but the training
   * data is defined by specifying the target point instead of the displacement vector
   */
  def posteriorMean(trainingData: IndexedSeq[(PointId, Point[D])], sigma2: Double): DDomain[D] = {
    val trainingDataWithDisplacements = trainingData.map { case (id, targetPoint) =>
      (id, targetPoint - reference.pointSet.point(id))
    }
    val mean = gp.posteriorMean(trainingDataWithDisplacements, sigma2)
    warper.transformWithField(gp.domain, mean)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posteriorMean(Int, Point[_3D], Double)]]], but the training data is
   * defined by specifying the target point instead of the displacement vector
   */
  def posteriorMean(
    trainingData: IndexedSeq[(PointId, Point[D], MultivariateNormalDistribution)]
  ): DDomain[D] = {
    val trainingDataWithDisplacements = trainingData.map { case (id, targetPoint, cov) =>
      (id, targetPoint - reference.pointSet.point(id), cov)
    }
    val mean = gp.posteriorMean(trainingDataWithDisplacements)
    warper.transformWithField(gp.domain, mean)
  }

  /**
   * transform the statistical mesh model using the given rigid transform. The spanned shape space is not affected by
   * this operations.
   */
  def transform(rigidTransform: RigidTransformation[D]): PointDistributionModel[D, DDomain] = {
    val newRef = warper.transform(reference, rigidTransform)

    val newMean: DenseVector[Double] = {
      val newMeanVecs = for ((pt, meanAtPoint) <- gp.mean.pointsWithValues) yield {
        rigidTransform(pt + meanAtPoint) - rigidTransform(pt)
      }
      val data = newMeanVecs.map(_.toArray).flatten.toArray
      DenseVector(data)
    }

    val newBasisMat = DenseMatrix.zeros[Double](gp.basisMatrix.rows, gp.basisMatrix.cols)

    for ((Eigenpair(_, ithKlBasis), i) <- gp.klBasis.zipWithIndex) {
      val newIthBasis = for ((pt, basisAtPoint) <- ithKlBasis.pointsWithValues) yield {
        rigidTransform(pt + basisAtPoint) - rigidTransform(pt)
      }
      val data = newIthBasis.map(_.toArray).flatten.toArray
      newBasisMat(::, i) := DenseVector(data)
    }
    val newGp = new DiscreteLowRankGaussianProcess[D, DDomain, EuclideanVector[D]](
      warper.transform(gp.domain, rigidTransform),
      newMean,
      gp.variance,
      newBasisMat
    )

    PointDistributionModel(newGp)

  }

  /**
   * Warps the reference mesh with the given transform. The space spanned by the model is not affected.
   */
  def changeReference(t: Point[D] => Point[D]): PointDistributionModel[D, DDomain] = {

    val newRef = warper.transform(reference, Transformation(t))
    val newMean = gp.mean.pointsWithValues.map { case (refPt, meanVec) => (refPt - t(refPt)) + meanVec }
    val newMeanVec = DenseVector(newMean.map(_.toArray).flatten.toArray)
    val newGp = new DiscreteLowRankGaussianProcess[D, DDomain, EuclideanVector[D]](newRef,
                                                                                   newMeanVec,
                                                                                   gp.variance,
                                                                                   gp.basisMatrix
    )
    PointDistributionModel(newGp)
  }

  def newReference[NewDomain[DD] <: DiscreteDomain[DD]](
    newReference: NewDomain[D],
    interpolator: FieldInterpolator[D, DDomain, EuclideanVector[D]]
  )(implicit canWarp: DomainWarp[D, NewDomain]): PointDistributionModel[D, NewDomain] = {
    val newGP = gp.interpolate(interpolator).discretize[NewDomain](newReference)
    PointDistributionModel(newGP)
  }

}

object PointDistributionModel {

  /**
   * creates a StatisticalMeshModel by discretizing the given Gaussian Process on the points of the reference mesh.
   */
  def apply[D: NDSpace, PointRepr[D] <: DiscreteDomain[D]](
    reference: PointRepr[D],
    gp: LowRankGaussianProcess[D, EuclideanVector[D]]
  )(implicit
    canWarp: DomainWarp[D, PointRepr],
    vectorizer: Vectorizer[EuclideanVector[D]]
  ): PointDistributionModel[D, PointRepr] = {
    val discreteGp = DiscreteLowRankGaussianProcess(reference, gp)
    new PointDistributionModel(discreteGp)
  }

  /**
   * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
   *
   * @see
   *   [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Double], DenseVector[Double], DenseMatrix[Double]]
   */
  private[scalismo] def apply[D: NDSpace, PointRepr[D] <: DiscreteDomain[D]](
    reference: PointRepr[D],
    meanVector: DenseVector[Double],
    variance: DenseVector[Double],
    basisMatrix: DenseMatrix[Double]
  )(implicit
    canWarp: DomainWarp[D, PointRepr],
    vectorizer: Vectorizer[EuclideanVector[D]]
  ): PointDistributionModel[D, PointRepr] = {
    val gp =
      new DiscreteLowRankGaussianProcess[D, PointRepr, EuclideanVector[D]](reference, meanVector, variance, basisMatrix)
    new PointDistributionModel(gp)
  }

  /**
   * Adds a bias model to the given statistical shape model
   */
  def augmentModel[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](
    model: PointDistributionModel[D, DDomain],
    biasModel: LowRankGaussianProcess[D, EuclideanVector[D]]
  )(implicit
    canWarp: DomainWarp[D, DDomain],
    vectorizer: Vectorizer[EuclideanVector[D]]
  ): PointDistributionModel[D, DDomain] = {

    val discretizedBiasModel = biasModel.discretize(model.reference)
    val eigenvalues = DenseVector.vertcat(model.gp.variance, discretizedBiasModel.variance).map(sqrt(_))
    val eigenvectors = DenseMatrix.horzcat(model.gp.basisMatrix, discretizedBiasModel.basisMatrix)

    for (i <- 0 until eigenvalues.length) {
      eigenvectors(::, i) :*= eigenvalues(i)
    }

    val l: DenseMatrix[Double] = eigenvectors.t * eigenvectors
    val SVD(v, _, _) = breeze.linalg.svd(l)
    val U: DenseMatrix[Double] = eigenvectors * v
    val d: DenseVector[Double] = DenseVector.zeros(U.cols)
    for (i <- 0 until U.cols) {
      d(i) = breeze.linalg.norm(U(::, i))
      U(::, i) := U(::, i) * (1.0 / d(i))
    }

    val augmentedGP = new DiscreteLowRankGaussianProcess[D, DDomain, EuclideanVector[D]](
      model.gp.domain,
      meanVector = model.gp.meanVector + discretizedBiasModel.meanVector,
      variance = breeze.numerics.pow(d, 2),
      basisMatrix = U
    )
    new PointDistributionModel(augmentedGP)
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence. All points of the reference
   * mesh are considered for computing the PCA
   *
   * Per default, the resulting mesh model will have rank (i.e. number of principal components) corresponding to the
   * number of linearly independent fields. By providing an explicit stopping criterion, one can, however, compute only
   * the leading principal components. See PivotedCholesky.StoppingCriterion for more details.
   */
  def createUsingPCA[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](
    dc: DataCollection[D, DDomain, EuclideanVector[D]],
    stoppingCriterion: PivotedCholesky.StoppingCriterion = PivotedCholesky.RelativeTolerance(0)
  )(implicit
    canWarp: DomainWarp[D, DDomain],
    vectorizer: Vectorizer[EuclideanVector[D]]
  ): PointDistributionModel[D, DDomain] = {
    if (dc.size < 3) {
      throw new IllegalArgumentException(s"The datacollection contains only ${dc.size} items. At least 3 are needed")
    }

    val fields = dc.fields(NearestNeighborInterpolator())
    createUsingPCA(dc.reference, fields, stoppingCriterion)
  }

  /**
   * Creates a new Statistical mesh model, with its mean and covariance matrix estimated from the given fields.
   *
   * Per default, the resulting mesh model will have rank (i.e. number of principal components) corresponding to the
   * number of linearly independent fields. By providing an explicit stopping criterion, one can, however, compute only
   * the leading principal components. See PivotedCholesky.StoppingCriterion for more details.
   */
  def createUsingPCA[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](
    reference: DDomain[D],
    fields: Seq[Field[D, EuclideanVector[D]]],
    stoppingCriterion: PivotedCholesky.StoppingCriterion
  )(implicit
    canWarp: DomainWarp[D, DDomain],
    vectorizer: Vectorizer[EuclideanVector[D]]
  ): PointDistributionModel[D, DDomain] = {

    val dgp: DiscreteLowRankGaussianProcess[D, DDomain, EuclideanVector[D]] =
      DiscreteLowRankGaussianProcess.createUsingPCA(reference, fields, stoppingCriterion)
    new PointDistributionModel(dgp)
  }

}

object PointDistributionModel1D {

  def apply[PointRepr[D] <: DiscreteDomain[D]](
    reference: PointRepr[_1D],
    gp: LowRankGaussianProcess[_1D, EuclideanVector[_1D]]
  )(implicit
    canWarp: DomainWarp[_1D, PointRepr],
    vectorizer: Vectorizer[EuclideanVector[_1D]]
  ): PointDistributionModel[_1D, PointRepr] = {
    val discreteGp = DiscreteLowRankGaussianProcess(reference, gp)
    new PointDistributionModel(discreteGp)
  }
}

object PointDistributionModel2D {

  def apply[PointRepr[D] <: DiscreteDomain[D]](
    reference: PointRepr[_2D],
    gp: LowRankGaussianProcess[_2D, EuclideanVector[_2D]]
  )(implicit
    canWarp: DomainWarp[_2D, PointRepr],
    vectorizer: Vectorizer[EuclideanVector[_2D]]
  ): PointDistributionModel[_2D, PointRepr] = {
    val discreteGp = DiscreteLowRankGaussianProcess(reference, gp)
    new PointDistributionModel(discreteGp)
  }
}

object PointDistributionModel3D {

  def apply[PointRepr[D] <: DiscreteDomain[D]](
    reference: PointRepr[_3D],
    gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]
  )(implicit
    canWarp: DomainWarp[_3D, PointRepr],
    vectorizer: Vectorizer[EuclideanVector[_3D]]
  ): PointDistributionModel[_3D, PointRepr] = {
    val discreteGp = DiscreteLowRankGaussianProcess(reference, gp)
    new PointDistributionModel(discreteGp)
  }
}
