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

import java.io.File
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.common.Field
import scalismo.geometry._
import scalismo.io.MeshIO
import scalismo.kernels.{ DiagonalKernel, GaussianKernel }
import scalismo.mesh.{ MeshMetrics, TriangleMesh }
import scalismo.numerics.UniformMeshSampler3D
import scalismo.registration.{ LandmarkRegistration, TranslationTransform }
import scalismo.statisticalmodel.{ GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel }
import scalismo.utils.Random

class DataCollectionTests extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("A datacollection") {

    val transformations = for (i <- 0 until 10) yield TranslationTransform(EuclideanVector(i.toDouble, 0.0, 0.0))
    val dataItems = for ((t, i) <- transformations.zipWithIndex) yield DataItem(s"transformation-$i", t)
    val meshPath = getClass.getResource("/facemesh.stl").getPath
    val referenceMesh = MeshIO.readMesh(new File(URLDecoder.decode(meshPath, "UTF-8"))).get

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

    it("returns a mean that is closer to in average distance to all other items than the other items") {
      def averageDistanceToAllMeshes(testMesh: TriangleMesh[_3D]) = {
        val distancesMeshesInDC = for (transformation <- dataCollection.dataItems.map(_.transformation)) yield {
          val mesh = dataCollection.reference.transform(transformation)
          MeshMetrics.procrustesDistance(mesh, testMesh)
        }
        distancesMeshesInDC.sum / distancesMeshesInDC.size
      }

      val avgDistForMean = averageDistanceToAllMeshes(dataCollection.meanSurface)

      for (transformation <- dataCollection.dataItems.map(_.transformation)) {
        val mesh = dataCollection.reference.transform(transformation)

        if (averageDistanceToAllMeshes(mesh) > 1e-10) {
          averageDistanceToAllMeshes(mesh) should be > (avgDistForMean)
        }
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

  object Fixture {

    val path: String = getClass.getResource("/nonAlignedFaces").getPath
    val nonAlignedFaces = new File(URLDecoder.decode(path, "UTF-8")).listFiles.sortBy(_.getName).map { f => MeshIO.readMesh(f).get }.toIndexedSeq
    val ref = nonAlignedFaces.head
    val dataset = nonAlignedFaces.tail

    val aligendDataset = dataset.map { d =>
      val trans = LandmarkRegistration.rigid3DLandmarkRegistration((d.pointSet.points zip ref.pointSet.points).toIndexedSeq, Point(0, 0, 0))
      d.transform(trans)
    }

    val trainingSet = aligendDataset.drop(3)
    val testingSet = aligendDataset.take(3)

    val dc = DataCollection.fromMeshSequence(ref, trainingSet)._1.get
    val pcaModel = StatisticalMeshModel.createUsingPCA(dc).get
    val testDC = DataCollection.fromMeshSequence(pcaModel.referenceMesh, testingSet)._1.get

  }

  describe("GPA") {
    it("leads to a data collection that is on average closer to the mean than the original data collection") {
      val gpaDC = DataCollection.gpa(Fixture.dc, maxIteration = 3)
      val gpaMean = gpaDC.meanSurface

      def surfaceDist(mesh1: TriangleMesh[_3D], mesh2: TriangleMesh[_3D]): Double = {
        val dists = for ((pt1, pt2) <- mesh1.pointSet.points.zip(mesh2.pointSet.points)) yield {
          (pt1 - pt2).norm
        }
        dists.sum
      }

      def averageDistanceToAllMeshes(dc: DataCollection, testMesh: TriangleMesh[_3D]) = {
        val distancesMeshesInDC = for (dataItem <- dc.dataItems) yield {
          val mesh = dc.reference.transform(dataItem.transformation)
          surfaceDist(mesh, testMesh)
        }
        distancesMeshesInDC.sum / distancesMeshesInDC.size
      }

      averageDistanceToAllMeshes(Fixture.dc, Fixture.dc.meanSurface) should be > averageDistanceToAllMeshes(gpaDC, gpaMean)
    }

    it("keeps the reference shape unchanged") {
      val gpaDC = DataCollection.gpa(Fixture.dc)

      gpaDC.reference should equal(Fixture.dc.reference)
    }

  }

  describe("Generalization") {

    val zeroMean = Field(Fixture.dc.reference.boundingBox, (pt: Point[_3D]) => EuclideanVector(0, 0, 0))
    val matrixValuedGaussian = DiagonalKernel(GaussianKernel[_3D](25) * 20, 3)
    val bias: GaussianProcess[_3D, EuclideanVector[_3D]] = GaussianProcess(zeroMean, matrixValuedGaussian)
    val biasLowRank = LowRankGaussianProcess.approximateGPNystrom(bias, UniformMeshSampler3D(Fixture.pcaModel.referenceMesh, 500), Fixture.pcaModel.rank + 5)
    val augmentedModel = StatisticalMeshModel.augmentModel(Fixture.pcaModel, biasLowRank)

    it("gives the same values when evaluated 10 times on normal PCA Model") {
      val gens = (0 until 10) map { _ => ModelMetrics.generalization(Fixture.pcaModel, Fixture.testDC).get }
      assert(gens.forall(_ - gens(0) < 1.0e-14))
    }

    it("gives the same values when evaluated 10 times on augmented model") {
      val gens = (0 until 10) map { _ => ModelMetrics.generalization(augmentedModel, Fixture.testDC).get }
      assert(gens.forall(_ - gens(0) < 1.0e-14))
    }

    it("improves when the model is augmented with a Gaussian") {

      (0 until 10) foreach { i =>
        val genAugmented = ModelMetrics.generalization(augmentedModel, Fixture.testDC).get
        val genOriginal = ModelMetrics.generalization(Fixture.pcaModel, Fixture.testDC).get

        assert(genAugmented < genOriginal)
      }
    }
  }

}



class DataCollectionMeshVoulmeTests extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("A datacollection Volume Mesh") {

    val transformations = for (i <- 0 until 10) yield TranslationTransform(EuclideanVector(i.toDouble, 0.0, 0.0))
    val dataItems = for ((t, i) <- transformations.zipWithIndex) yield DataItem(s"transformation-$i", t)
    val meshPath = getClass.getResource("/tetraMesh.vtu").getPath
    val referenceMesh = MeshIO.readTetrahedralMesh(new File(URLDecoder.decode(meshPath,"UTF-8"))).get

    val dataCollection = DataCollectionOfMeshVolume(referenceMesh, dataItems)

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
