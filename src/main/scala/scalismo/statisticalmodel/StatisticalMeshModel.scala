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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.common.interpolation.{NearestNeighborInterpolator3D, TriangleMeshInterpolator}
import scalismo.geometry.EuclideanVector._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.numerics.PivotedCholesky
import scalismo.transformations.RigidTransformation
import scalismo.statisticalmodel.dataset.DataCollection.TriangleMeshDataCollection
import scalismo.utils.Random

import scala.util.Try

/**
 * A StatisticalMeshModel is isomorphic to a [[DiscreteLowRankGaussianProcess]]. The difference is that while the DiscreteLowRankGaussianProcess
 * models defomation fields, the StatisticalMeshModel applies the deformation fields to a mesh, and warps the mesh with the deformation fields to
 * produce a new mesh.
 *
 * @see [[DiscreteLowRankGaussianProcess]]
 */
case class StatisticalMeshModel private (referenceMesh: TriangleMesh[_3D],
                                         gp: DiscreteLowRankGaussianProcess[_3D, TriangleMesh, EuclideanVector[_3D]]) {

  private val pdm = PointDistributionModel[_3D, TriangleMesh](gp)

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank: Int = pdm.rank

  /**
   * The mean shape
   * @see [[DiscreteLowRankGaussianProcess.mean]]
   */
  lazy val mean: TriangleMesh[_3D] = pdm.mean

  /**
   * The covariance between two points of the  mesh with given point id.
   * @see [[DiscreteLowRankGaussianProcess.cov]]
   */
  def cov(ptId1: PointId, ptId2: PointId): DenseMatrix[Double] = pdm.cov(ptId1, ptId2)

  /**
   * draws a random shape.
   * @see [[DiscreteLowRankGaussianProcess.sample]]
   */
  def sample()(implicit rand: Random): TriangleMesh3D = pdm.sample()

  /**
   * returns the probability density for an instance of the model
   * @param instanceCoefficients coefficients of the instance in the model. For shapes in correspondence, these can be obtained using the coefficients method
   *
   */
  def pdf(instanceCoefficients: DenseVector[Double]): Double = pdm.pdf(instanceCoefficients)

  /**
   * returns a shape that corresponds to a linear combination of the basis functions with the given coefficients c.
   *  @see [[DiscreteLowRankGaussianProcess.instance]]
   */
  def instance(c: DenseVector[Double]): TriangleMesh[_3D] = pdm.instance(c)

  /**
   *  Returns a marginal StatisticalMeshModel, modelling deformations only on the chosen points of the reference
   *
   *  This method proceeds by clipping the reference mesh to keep only the indicated point identifiers, and then marginalizing the
   *  GP over those points. Notice that when clipping, not all indicated point ids will be part of the clipped mesh, as some points may not belong
   *  to any cells anymore. Therefore 2 behaviours are supported by this method :
   *
   *  1- in case some of the indicated pointIds remain after clipping and do form a mesh, a marginal model is returned only for those points
   *  2- in case none of the indicated points remain (they are not meshed), a reference mesh with all indicated point Ids and no cells is constructed and a marginal
   *  over this new reference is returned
   *
   * @see [[DiscreteLowRankGaussianProcess.marginal]]
   */
  def marginal(ptIds: IndexedSeq[PointId]): StatisticalMeshModel = {
    val newRef: TriangleMesh[_3D] = referenceMesh.operations.maskPoints(f => ptIds.contains(f)).transformedMesh
    val marginalModel = pdm.newReference(newRef, NearestNeighborInterpolator3D())
    new StatisticalMeshModel(marginalModel.reference, marginalModel.gp)
  }

  /**
   * Returns a reduced rank model, using only the leading basis functions.
   *
   * @param newRank: The rank of the new model.
   */
  def truncate(newRank: Int): StatisticalMeshModel = {
    val truncate = pdm.truncate(newRank)
    new StatisticalMeshModel(truncate.reference, truncate.gp)
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.project]]
   */
  def project(mesh: TriangleMesh[_3D]): TriangleMesh3D = pdm.project(mesh)

  /**
   * @see [[DiscreteLowRankGaussianProcess.coefficients]]
   */
  def coefficients(mesh: TriangleMesh[_3D]): DenseVector[Double] = pdm.coefficients(mesh)

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D])], sigma2: Double)]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(PointId, Point[_3D])], sigma2: Double): StatisticalMeshModel = {
    val posterior = pdm.posterior(trainingData, sigma2)
    new StatisticalMeshModel(posterior.reference, posterior.gp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D], Double)]]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(
    trainingData: IndexedSeq[(PointId, Point[_3D], MultivariateNormalDistribution)]
  ): StatisticalMeshModel = {
    val posterior = pdm.posterior(trainingData)
    new StatisticalMeshModel(posterior.reference, posterior.gp)
  }

  /**
   * transform the statistical mesh model using the given rigid transform.
   * The spanned shape space is not affected by this operations.
   */
  def transform(rigidTransform: RigidTransformation[_3D]): StatisticalMeshModel = {
    val transformModel = pdm.transform(rigidTransform)
    new StatisticalMeshModel(transformModel.reference, transformModel.gp)
  }

  /**
   * Warps the reference mesh with the given transform. The space spanned by the model is not affected.
   */
  def changeReference(t: Point[_3D] => Point[_3D]): StatisticalMeshModel = {
    val changeRef = pdm.changeReference(t)
    new StatisticalMeshModel(changeRef.reference, changeRef.gp)
  }

  /**
   * Changes the number of vertices on which the model is defined
   * @param targetNumberOfVertices  The desired number of vertices
   * @return The new model
   */
  def decimate(targetNumberOfVertices: Int): StatisticalMeshModel = {
    val newReference = referenceMesh.operations.decimate(targetNumberOfVertices)
    val interpolator = TriangleMeshInterpolator[EuclideanVector[_3D]]()
    val newGp = gp.interpolate(interpolator)

    StatisticalMeshModel(newReference, newGp)
  }
}

object StatisticalMeshModel {

  /**
   * creates a StatisticalMeshModel by discretizing the given Gaussian Process on the points of the reference mesh.
   */
  def apply(referenceMesh: TriangleMesh[_3D],
            gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]): StatisticalMeshModel = {
    val discreteGp = DiscreteLowRankGaussianProcess(referenceMesh, gp)
    new StatisticalMeshModel(referenceMesh, discreteGp)
  }

  /**
   * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
   *
   * @see [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Double], DenseVector[Double], DenseMatrix[Double]]
   */
  private[scalismo] def apply(referenceMesh: TriangleMesh[_3D],
                              meanVector: DenseVector[Double],
                              variance: DenseVector[Double],
                              basisMatrix: DenseMatrix[Double]): StatisticalMeshModel = {
    val pdModel = PointDistributionModel(referenceMesh, meanVector, variance, basisMatrix)
    new StatisticalMeshModel(pdModel.reference, pdModel.gp)
  }

  /**
   *  Adds a bias model to the given statistical shape model
   */
  def augmentModel(model: StatisticalMeshModel,
                   biasModel: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]): StatisticalMeshModel = {
    val pdModel = PointDistributionModel(model.gp)
    val augmentModel = PointDistributionModel.augmentModel(pdModel, biasModel)
    new StatisticalMeshModel(augmentModel.reference, augmentModel.gp)
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence.
   * All points of the reference mesh are considered for computing the PCA
   *
   * Per default, the resulting mesh model will have rank (i.e. number of principal components) corresponding to
   * the number of linearly independent fields. By providing an explicit stopping criterion, one can, however,
   * compute only the leading principal components. See PivotedCholesky.StoppingCriterion for more details.
   */
  def createUsingPCA(
    dc: TriangleMeshDataCollection[_3D],
    stoppingCriterion: PivotedCholesky.StoppingCriterion = PivotedCholesky.RelativeTolerance(0)
  ): Try[StatisticalMeshModel] = {
    Try {
      val pdm = PointDistributionModel.createUsingPCA(dc, stoppingCriterion)
      new StatisticalMeshModel(pdm.reference, pdm.gp)
    }
  }

  /**
   * Creates a new Statistical mesh model, with its mean and covariance matrix estimated from the given fields.
   *
   * Per default, the resulting mesh model will have rank (i.e. number of principal components) corresponding to
   * the number of linearly independent fields. By providing an explicit stopping criterion, one can, however,
   * compute only the leading principal components. See PivotedCholesky.StoppingCriterion for more details.
   *
   */
  def createUsingPCA(referenceMesh: TriangleMesh[_3D],
                     fields: Seq[Field[_3D, EuclideanVector[_3D]]],
                     stoppingCriterion: PivotedCholesky.StoppingCriterion): StatisticalMeshModel = {
    val pdm = PointDistributionModel.createUsingPCA(referenceMesh, fields, stoppingCriterion)
    new StatisticalMeshModel(pdm.reference, pdm.gp)
  }

}
