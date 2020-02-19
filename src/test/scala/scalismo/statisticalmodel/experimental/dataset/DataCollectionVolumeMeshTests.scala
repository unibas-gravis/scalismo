package scalismo.statisticalmodel.experimental.dataset

import java.io.File
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.io.MeshIO
import scalismo.registration.TranslationTransform
import scalismo.statisticalmodel.dataset.DataItem
import scalismo.utils.Random

class DataCollectionVolumeMeshTests extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("A datacollection Volume Mesh") {

    val transformations = for (i <- 0 until 10) yield TranslationTransform(EuclideanVector(i.toDouble, 0.0, 0.0))
    val dataItems = for ((t, i) <- transformations.zipWithIndex) yield DataItem(s"transformation-$i", t)
    val meshPath = getClass.getResource("/tetraMesh.vtu").getPath
    val referenceMesh = MeshIO.readTetrahedralMesh(new File(URLDecoder.decode(meshPath, "UTF-8"))).get

    val dataCollection = DataCollectionOfVolumeMesh(referenceMesh, dataItems)

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

    it("returns a mean surface, which is the arithmetic mean of the meshes represented by the collection") {
      val computedMean = dataCollection.meanSurface
      val meshes = dataCollection.dataItems.map(di => dataCollection.reference.transform(di.transformation))

      for (pointId <- util.Random.shuffle(dataCollection.reference.pointSet.pointIds.toIndexedSeq).take(100)) {
        val pointsOnMeshes = meshes.map(mesh => mesh.pointSet.point(pointId))
        val meanOfPoint = pointsOnMeshes.foldLeft(Point(0, 0, 0))((acc, p) => acc + p.toVector / pointsOnMeshes.size)
        (computedMean.pointSet.point(pointId) - meanOfPoint).norm should be < 1e-5
      }
    }

  }

}
