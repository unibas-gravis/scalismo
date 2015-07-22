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

import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.common.DiscreteVectorField
import scalismo.geometry.{ Point, _3D }
import scalismo.mesh.{ Mesh, TriangleMesh }
import scalismo.registration.{ RigidTransformation, Transformation }
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.Eigenpair
import scalismo.mesh.TriangleCell

/**
 * A StatisticalMeshModel is isomorphic to a [[DiscreteLowRankGaussianProcess]]. The difference is that while the DiscreteLowRankGaussianProcess
 * models defomation fields, the StatisticalMeshModel applies the deformation fields to a mesh, and warps the mesh with the deformation fields to
 * produce a new mesh.
 *
 * @see [[DiscreteLowRankGaussianProcess]]
 */
case class StatisticalMeshModel private (val referenceMesh: TriangleMesh, val gp: DiscreteLowRankGaussianProcess[_3D, _3D]) {

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank = gp.rank

  /**
   * The mean shape
   * @see [[DiscreteLowRankGaussianProcess.mean]]
   */
  def mean: TriangleMesh = warpReference(gp.mean)

  /**
   * The covariance between two points of the  mesh with given point id.
   * @see [[DiscreteLowRankGaussianProcess.cov]]
   */
  def cov(ptId1: Int, ptId2: Int) = gp.cov(ptId1, ptId2)

  /**
   * draws a random shape.
   * @see [[DiscreteLowRankGaussianProcess.sample]]
   */
  def sample = warpReference(gp.sample)

  /**
   * returns the probability density for an instance of the model
   * @param instanceCoefficients coefficients of the instance in the model. For shapes in correspondence, these can be obtained using the coefficients method
   *
   */
  def pdf(instanceCoefficients: DenseVector[Float]): Double = {
    val disVecField = gp.instance(instanceCoefficients)
    gp.pdf(disVecField)
  }

  /**
   * returns a shape that corresponds to a linear combination of the basis functions with the given coefficients c.
   *  @see [[DiscreteLowRankGaussianProcess.instance]]
   */
  def instance(c: DenseVector[Float]): TriangleMesh = warpReference(gp.instance(c))

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
  def marginal(ptIds: IndexedSeq[Int]) = {
    val clippedReference = Mesh.clipMesh(referenceMesh, p => { !ptIds.contains(referenceMesh.findClosestPoint(p)._2) })
    // not all of the ptIds remain in the reference after clipping, since their cells might disappear
    val remainingPtIds = clippedReference.points.map(p => referenceMesh.findClosestPoint(p)._2).toIndexedSeq
    if (remainingPtIds.size == 0) {
      val newRef = TriangleMesh(ptIds.map(id => referenceMesh.point(id)), IndexedSeq[TriangleCell]())
      val marginalGP = gp.marginal(ptIds.toIndexedSeq)
      StatisticalMeshModel(newRef, marginalGP)
    } else {
      val marginalGP = gp.marginal(remainingPtIds.toIndexedSeq)
      StatisticalMeshModel(clippedReference, marginalGP)
    }
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.project]]
   */
  def project(mesh: TriangleMesh) = {
    val displacements = referenceMesh.points.zip(mesh.points).map({ case (refPt, tgtPt) => tgtPt - refPt }).toIndexedSeq
    val dvf = DiscreteVectorField(referenceMesh, displacements)
    warpReference(gp.project(dvf))
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.coefficients]]
   */
  def coefficients(mesh: TriangleMesh): DenseVector[Float] = {
    val displacements = referenceMesh.points.zip(mesh.points).map({ case (refPt, tgtPt) => tgtPt - refPt }).toIndexedSeq
    val dvf = DiscreteVectorField(referenceMesh, displacements)
    gp.coefficients(dvf)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D])], sigma2: Double)]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(Int, Point[_3D])], sigma2: Double): StatisticalMeshModel = {
    val trainingDataWithDisplacements = trainingData.map { case (id, targetPoint) => (id, targetPoint - referenceMesh.point(id)) }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements, sigma2)
    new StatisticalMeshModel(referenceMesh, posteriorGp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D], Double)]]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(Int, Point[_3D], NDimensionalNormalDistribution[_3D])]): StatisticalMeshModel = {
    val trainingDataWithDisplacements = trainingData.map { case (id, targetPoint, cov) => (id, targetPoint - referenceMesh.point(id), cov) }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements)
    new StatisticalMeshModel(referenceMesh, posteriorGp)
  }

  /**
   * transform the statistical mesh model using the given rigid transform.
   * The spanned shape space is not affected by this operations.
   */
  def transform(rigidTransform: RigidTransformation[_3D]): StatisticalMeshModel = {
    val newRef = referenceMesh.transform(rigidTransform)

    val newMean: DenseVector[Float] = {
      val newMeanVecs = for ((pt, meanAtPoint) <- gp.mean.pointsWithValues) yield {
        rigidTransform(pt + meanAtPoint) - rigidTransform(pt)
      }
      val data = newMeanVecs.map(_.toArray).flatten.toArray
      DenseVector(data)
    }

    val newBasisMat = DenseMatrix.zeros[Float](gp.basisMatrix.rows, gp.basisMatrix.cols)

    for ((Eigenpair(_, ithKlBasis), i) <- gp.klBasis.zipWithIndex) {
      val newIthBasis = for ((pt, basisAtPoint) <- ithKlBasis.pointsWithValues) yield {
        rigidTransform(pt + basisAtPoint) - rigidTransform(pt)
      }
      val data = newIthBasis.map(_.toArray).flatten.toArray
      newBasisMat(::, i) := DenseVector(data)
    }
    val newGp = new DiscreteLowRankGaussianProcess[_3D, _3D](gp.domain.transform(rigidTransform), newMean, gp.variance, newBasisMat)

    new StatisticalMeshModel(newRef, newGp)

  }

  /**
   * Warps the reference mesh with the given transform. The space spanned by the model is not affected.
   */
  def changeReference(t: Point[_3D] => Point[_3D]): StatisticalMeshModel = {

    val newRef = referenceMesh.transform(t)
    val newMean = gp.mean.pointsWithValues.map { case (refPt, meanVec) => (refPt - t(refPt)) + meanVec }
    val newMeanVec = DenseVector(newMean.map(_.toArray).flatten.toArray)
    val newGp = new DiscreteLowRankGaussianProcess[_3D, _3D](newRef, newMeanVec, gp.variance, gp.basisMatrix)
    new StatisticalMeshModel(newRef, newGp)
  }

  private def warpReference(vectorPointData: DiscreteVectorField[_3D, _3D]) = {
    val newPoints = vectorPointData.pointsWithValues.map { case (pt, v) => pt + v }
    new TriangleMesh(newPoints.toIndexedSeq, referenceMesh.cells, Some(referenceMesh.cellMap))
  }

}

object StatisticalMeshModel {

  /**
   * creates a StatisticalMeshModel by discretizign the given Gaussian Process on the points of the reference mesh.
   */
  def apply(referenceMesh: TriangleMesh, gp: LowRankGaussianProcess[_3D, _3D]): StatisticalMeshModel = {
    val discreteGp = DiscreteLowRankGaussianProcess(referenceMesh, gp)
    new StatisticalMeshModel(referenceMesh, discreteGp)
  }

  /**
   * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
   * @see [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Float], DenseVector[Float], DenseMatrix[Float]]
   */
  private[scalismo] def apply(referenceMesh: TriangleMesh, meanVector: DenseVector[Float], variance: DenseVector[Float], basisMatrix: DenseMatrix[Float]) = {
    val gp = new DiscreteLowRankGaussianProcess[_3D, _3D](referenceMesh, meanVector, variance, basisMatrix)
    new StatisticalMeshModel(referenceMesh, gp)
  }

  /**
   * Creates a new DiscreteLowRankGaussianProcess, where the mean and covariance matrix are estimated from the given transformations.
   *
   */
  def createStatisticalMeshModelFromTransformations(referenceMesh: TriangleMesh, transformations: Seq[Transformation[_3D]]): StatisticalMeshModel = {
    val dgp = DiscreteLowRankGaussianProcess.createDiscreteLowRankGPFromTransformations(referenceMesh, transformations)
    new StatisticalMeshModel(referenceMesh, dgp)
  }

}

