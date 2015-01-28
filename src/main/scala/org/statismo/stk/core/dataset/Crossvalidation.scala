package org.statismo.stk.core.dataset

import scala.util.Try
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.statisticalmodel.LowRankGaussianProcess
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel
import org.statismo.stk.core.statisticalmodel.GaussianProcess
import org.statismo.stk.core.statisticalmodel.DiscreteLowRankGaussianProcess

/**
 * Implements utility functions for evaluating the quality of a registered dataset
 */
object Crossvalidation {

  type EvaluationFunction[A] = (StatisticalMeshModel, TriangleMesh) => A

  private def projectIntoModel(model: StatisticalMeshModel, mesh: TriangleMesh): TriangleMesh = {
    val ptPairs = model.referenceMesh.points.toIndexedSeq.zip(mesh.points.toIndexedSeq)

    val trainingData = ptPairs.zipWithIndex.map {
      case ((refPt, targetPt), idx) => (idx, targetPt)
    }
    model.project(trainingData, 1e-5)
  }

  /**
   * Perform a leave one out crossvalidation. See nFoldCrossvalidation for details
   */
  def leaveOneOutCrossvalidation[A](dataCollection: DataCollection, evalFun: EvaluationFunction[A], biasModel: Option[LowRankGaussianProcess[_3D, _3D]] = None) = {
    nFoldCrossvalidation(dataCollection.size, dataCollection, evalFun, biasModel)
  }

  /**
   * Perform an n-fold crossvalidation. Given the chosen number of folds, this method will repeatedly split the data collection
   * into a training and and a test set.
   * A [[StatisticalMeshModel]] is then built from the training set of each fold. In case a biasModel is provided, this model is always added to the model built from the training data.
   *
   * For each testing dataset in a fold, the evalFun is called to evaluate the quality of the model built from the training set.
   *
   * @returns a sequence the size of the chosen number of folds that contains the sequence of evaluations for each data item in the fold's testing set,
   * or an error if the model building for a fold failed.
   */
  def nFoldCrossvalidation[A](numFolds: Int, dc: DataCollection, evalFun: EvaluationFunction[A], biasModel: Option[LowRankGaussianProcess[_3D, _3D]] = None): Seq[Try[Seq[A]]] = {

    val folds = dc.createCrossValidationFolds(numFolds)
    val evalResultsForFolds = for (fold <- folds) yield {
      val td = fold.trainingData
      PCAModel.buildModelFromDataCollection(td).map { pcaModel =>
        val model = if (biasModel.isDefined) PCAModel.augmentModel(pcaModel, biasModel.get) else pcaModel
        val evalResults = for (testingItem <- fold.testingData.dataItems) yield {
          val testMesh = dc.reference.warp(testingItem.transformation)
          evalFun(model, testMesh)
        }
        evalResults
      }
    }
    evalResultsForFolds
  }
}
