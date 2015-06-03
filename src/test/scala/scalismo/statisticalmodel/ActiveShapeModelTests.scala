package scalismo.statisticalmodel

import java.io.File

import scalismo.ScalismoTestSuite
import scalismo.geometry._3D
import scalismo.io.{ ImageIO, MeshIO, StatismoIO }
import scalismo.mesh.{ MeshMetrics, TriangleMesh }
import scalismo.numerics.{ Sampler, UniformMeshSampler3D }
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.asm._
import scalismo.statisticalmodel.dataset.DataCollection

class ActiveShapeModelTests extends ScalismoTestSuite {

  describe("An active shape model") {
    it("Can be built, transformed and correctly fitted from/to artificial data") {

      val imagePreprocessor = GaussianGradientImagePreprocessor(0.1f)
      val featureExtractor = NormalDirectionFeatureExtractor(10, 1f)
      def samplerPerMesh(mesh: TriangleMesh): Sampler[_3D] = UniformMeshSampler3D(mesh, 1000, 42)
      val searchMethod = NormalDirectionSearchPointSampler(30, 6)
      val fittingConfig = FitConfiguration(featureDistanceThreshold = 2f, pointDistanceThreshold = 3f, modelCoefficientBounds = 3f)

      val shapeModel = StatismoIO.readStatismoMeshModel(new File(getClass.getResource(s"/asmData/model.h5").getPath)).get
      val nbFiles = 7
      val meshes = (0 until nbFiles).map(i => MeshIO.readMesh(new File(getClass.getResource(s"/asmData/$i.stl").getPath)).get)
      val images = (0 until nbFiles).map(i => ImageIO.read3DScalarImage[Float](new File(getClass.getResource(s"/asmData/$i.vtk").getPath)).get)

      val trainMeshes = meshes.drop(1)
      val trainImages = images.drop(1)
      val targetImage = images(0)
      val targetMesh = meshes(0)

      val dc = DataCollection.fromMeshSequence(shapeModel.referenceMesh, trainMeshes)._1.get
      val trainingData = trainImages zip dc.dataItems.map(_.transformation)

      val asm = ActiveShapeModel.trainModel(shapeModel, trainingData.toIterator, imagePreprocessor, featureExtractor, samplerPerMesh)

      // align the model
      val alignment = LandmarkRegistration.rigid3DLandmarkRegistration((asm.statisticalModel.mean.points zip targetMesh.points).toIndexedSeq)
      val alignedASM = asm.transform(alignment)

      // fit
      val fitIterator = alignedASM.fitIterator(targetImage, alignedASM.statisticalModel.mean, searchMethod, fittingConfig, 50)
      val fit = fitIterator.toSeq.last.get.mesh

      assert(MeshMetrics.diceCoefficient(fit, targetMesh) > 0.95)
    }
  }

}