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
package scalismo.statisticalmodel.dataset

import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.{GaussianProcess, StatisticalMeshModel}

import scala.util.Try


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
  def leaveOneOutCrossvalidation[A](dataCollection: DataCollection, evalFun: EvaluationFunction[A], biasModelAndRank: Option[(GaussianProcess[_3D, _3D],Int)] = None) = {
    nFoldCrossvalidation(dataCollection.size, dataCollection, evalFun, biasModelAndRank)
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
  def nFoldCrossvalidation[A](numFolds: Int, dc: DataCollection, evalFun: EvaluationFunction[A], biasModelAndRank: Option[(GaussianProcess[_3D, _3D], Int)] = None): Seq[Try[Seq[A]]] = {

    val folds = dc.createCrossValidationFolds(numFolds)
    val evalResultsForFolds = for (fold <- folds) yield {
      val td = fold.trainingData
      PCAModel.buildModelFromDataCollection(td).map { pcaModel =>
        val model = if (biasModelAndRank.isDefined) PCAModel.augmentModel(pcaModel, biasModelAndRank.get._1, biasModelAndRank.get._2) else pcaModel
        val evalResults = for (testingItem <- fold.testingData.dataItems) yield {
          val testMesh = dc.reference.transform(testingItem.transformation)
          evalFun(model, testMesh)
        }
        evalResults
      }
    }
    evalResultsForFolds
  }
}
