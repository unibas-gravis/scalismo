package org.statismo.stk.core.dataset

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import java.io.File
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.statisticalmodel.StatisticalMeshModel
import org.statismo.stk.core.io.StatismoIO
import breeze.linalg.DenseVector
import org.statismo.stk.core.mesh.TriangleMesh
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.mesh.TriangleCell
import org.statismo.stk.core.registration.TranslationTransform

class DataCollectionTests extends FunSpec with ShouldMatchers {
   org.statismo.stk.core.initialize()

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
