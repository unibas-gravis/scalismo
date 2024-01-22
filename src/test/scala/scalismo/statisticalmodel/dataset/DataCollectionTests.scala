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

import scalismo.ScalismoTestSuite
import scalismo.common.Field
import scalismo.geometry.*
import scalismo.io.MeshIO
import scalismo.kernels.{DiagonalKernel, GaussianKernel}
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.numerics.UniformMeshSampler3D
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel}
import scalismo.transformations.Translation
import scalismo.utils.Random

import java.io.File
import java.net.URLDecoder

class DataCollectionTests extends ScalismoTestSuite {

  implicit val rng: Random = Random(42L)

  describe("A datacollection") {

    val meshPath = getClass.getResource("/facemesh.stl").getPath
    val referenceMesh = MeshIO.readMesh(new File(URLDecoder.decode(meshPath, "UTF-8"))).get

    val transformations = for (i <- 0 until 10) yield Translation(EuclideanVector(i.toDouble, 0.0, 0.0))
    val translatedMeshes = for ((t, i) <- transformations.zipWithIndex) yield referenceMesh.transform(t)

    val dataCollection = DataCollection.fromTriangleMesh3DSequence(referenceMesh, translatedMeshes)

    it("yields the right number of cross-validation folds") {
      def createFolds(nFolds: Int) = {
        dataCollection.createCrossValidationFolds(nFolds)
      }

      createFolds(4).size should be(4)
      createFolds(2).size should be(2)
      dataCollection.createLeaveOneOutFolds.size should be(dataCollection.size)
    }

    it("yields the right fold sizes for a leave one out test") {
      for (fold <- dataCollection.createLeaveOneOutFolds) {
        fold.trainingData.size should be(dataCollection.size - 1)
        fold.testingData.size should be(1)
        fold.trainingData.dataItems.contains(fold.testingData) should be(false)
      }
    }

    it("has all distinct training datasets in a leave one out test") {
      val folds = dataCollection.createLeaveOneOutFolds
      for (fold <- folds) {
        fold.trainingData.dataItems.toSet.size should be(fold.trainingData.size)
      }

    }
  }

}
