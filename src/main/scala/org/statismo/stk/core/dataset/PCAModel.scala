package org.statismo.stk.core.dataset

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.statismo.stk.core.geometry.Point
import org.statismo.stk.core.geometry._3D
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.numerics.FixedPointsUniformMeshSampler3D
import org.statismo.stk.core.numerics.Sampler
import org.statismo.stk.core.registration.Transformation
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel
import org.statismo.stk.core.statisticalmodel.DiscreteLowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.GaussianProcess
import org.statismo.stk.core.kernels.MatrixValuedPDKernel

/**
 * Implements utility functions for building a [[StatisticalMeshModel]] from a [[DataCollection]] containing a reference and items in correspondence
 */
object PCAModel {

  type MeshSampler = Sampler[_3D]

  /**
   *  Adds a bias model to the given pca model
   */
  private[dataset] def augmentModel(pcaModel: StatisticalMeshModel, biasModel: LowRankGaussianProcess[_3D, _3D]): StatisticalMeshModel = {

    val modelGP = pcaModel.gp.interpolate(500)
    val newMean = (x: Point[_3D]) => modelGP.mean(x) + biasModel.mean(x)
    val newKLBasis = modelGP.klBasis ++ biasModel.klBasis
    val newGP = new LowRankGaussianProcess(pcaModel.referenceMesh.boundingBox, newMean, newKLBasis)

    StatisticalMeshModel(pcaModel.referenceMesh, newGP)
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence.
   * All points of the reference mesh are considered for computing the PCA
   */
  def buildModelFromDataCollection(dc: DataCollection): Try[StatisticalMeshModel] = {
    if (dc.size < 3) return Failure(new Throwable(s"We need to have at least 3 transformations to build a PCA Model (${dc.size} provied"))
    Success(StatisticalMeshModel.createStatisticalMeshModelFromTransformations(dc.reference, dc.dataItems.map(_.transformation)))
  }
}