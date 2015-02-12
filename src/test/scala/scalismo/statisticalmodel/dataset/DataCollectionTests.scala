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

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Matchers, FunSpec}
import java.io.File
import org.scalatest.matchers.ShouldMatchers
import breeze.linalg.DenseVector
import scalismo.io.MeshIO
import scalismo.registration.TranslationTransform
import scalismo.geometry._

class DataCollectionTests extends FunSpec with Matchers {
   scalismo.initialize()

  describe("A datacollection") {

    val transformations = for (i <- 0 until 10) yield TranslationTransform(Vector(i.toFloat, 0f, 0f))
    val dataItems = for ((t, i) <- transformations.zipWithIndex) yield DataItem(s"transformation-$i", t)
    val meshpath = getClass().getResource("/facemesh.h5").getPath()
    val referenceMesh = MeshIO.readMesh(new File(meshpath)).get

    val dataCollection = DataCollection(referenceMesh, dataItems)

    it("yields the right number of cross-validation folds") {
      def createFolds(nFolds: Int) = {
        dataCollection.createCrossValidationFolds(nFolds)
      }

      createFolds(1).size should be(1)
      createFolds(4).size should be(4)
      createFolds(2).size should be(2)
      dataCollection.createLeaveOneOutFolds.size should be(dataItems.size)
    }

    it("considers every dataset in a leave one out test") {
      val folds = dataCollection.createLeaveOneOutFolds

      // if we accumulated all the testing datasets, we should get all dataItems back. 
      val accumulatedTestingData = folds.foldLeft(Seq[DataItem[_3D]]())((acc, di) => acc :+ di.testingData.dataItems(0))
      val sortedAccTestData = accumulatedTestingData.sortWith((a, b) => a.info > b.info)
      val sortedDataItems = dataCollection.dataItems.sortWith((a, b) => a.info > b.info)
      sortedAccTestData should equal(sortedDataItems)

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
