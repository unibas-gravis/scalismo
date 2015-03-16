package scalismo.statisticalmodel.dataset

import scalismo.statisticalmodel.StatisticalMeshModel
import scalismo.mesh.TriangleMesh
import scalismo.mesh.Mesh
import scalismo.mesh.MeshMetrics
import scala.util.Try
import scala.util.Failure
import scala.util.Success

/**
 * Implements utility functions for evaluating the quality of a [[StatisticalMeshModel]]
 */
object ModelMetrics {

  /**
   * Returns the specificity metric of the Statistical Mesh Model, that is how close the model remains to the category of shapes it is supposed to represent
   * @param pcaModel model to be evaluated
   * @param data test data to verify specificity against
   * @param nbSamples number of samples drawn to compute the average
   *
   * The implementation of this metric is inspired from :
   * Styner, Martin A., et al. "Evaluation of 3D correspondence methods for model building." Information processing in medical imaging. Springer Berlin Heidelberg, 2003.
   *
   * The general idea is as follows :
   *
   *
   * 1 - sample a shape from the mesh model
   *
   *
   * 2-  compute the average mesh distance (see [[scalismo.mesh.MeshMetrics]]) of the sample to all elements of the given sequence of meshes and select the minimum distance
   *
   * These steps are then repeated nbSamples times and the average value is returned.
   *
   */

  def specificity(pcaModel: StatisticalMeshModel, data: Iterable[TriangleMesh], nbSamples: Int): Double = {

    (0 until nbSamples).par.map { _ =>
      val sample = pcaModel.sample
      data.map { m => MeshMetrics.avgDistance(m, sample) }.min
    }.sum * (1.0 / nbSamples)
  }

  /**
   * Returns the generalization metric of the Statistical Mesh Model, that is how well the model can represent unseen data
   *
   * @param pcaModel Statistical Mesh Model to be evaluated
   * @param dc test data collection that is in correspondence with the model reference
   *
   * The implementation of this metric is inspired from :
   * Styner, Martin A., et al. "Evaluation of 3D correspondence methods for model building." Information processing in medical imaging. Springer Berlin Heidelberg, 2003.
   *
   * For every mesh in the test data, we project the mesh into the model (that is find the closest shape in the model space to the given mesh) and compute the average mesh distance (see [[scalismo.mesh.MeshMetrics]]) between the mesh and the projection.
   * To be able to perform the projection, it is important that the data collection is in correspondence with the model.
   * The returned value is a scala.util.Try containing the average over all test data in case of success, or an Exception otherwise
   */

  def generalization(pcaModel: StatisticalMeshModel, dc: DataCollection): Try[Double] = {

    if (pcaModel.referenceMesh == dc.reference) Success {
      dc.dataItems.par.map { item =>
        val mesh = dc.reference.transform(item.transformation)
        val trainingData = (0 until pcaModel.referenceMesh.numberOfPoints) zip mesh.points.toIndexedSeq
        val projection = pcaModel.project(trainingData, 1e-6)
        MeshMetrics.avgDistance(projection, mesh)
      }.sum / dc.size.toDouble
    }
    else Failure(new Exception("pca model and test data collection must have the same reference"))
  }

}