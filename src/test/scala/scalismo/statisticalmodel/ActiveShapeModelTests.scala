package scalismo.statisticalmodel

import java.io.File

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.geometry.{ Point, _3D }
import scalismo.io.{ ImageIO, MeshIO, StatismoIO, StatisticalModelIO }
import scalismo.mesh.{ MeshMetrics, TriangleMesh }
import scalismo.numerics.{ Sampler, UniformMeshSampler3D }
import scalismo.registration.{ LandmarkRegistration }
import scalismo.statisticalmodel.asm._
import scalismo.statisticalmodel.dataset.DataCollection
import scalismo.utils.Random

class ActiveShapeModelTests extends ScalismoTestSuite {

  describe("An active shape model") {

    implicit val random = Random(42)

    object Fixture {
      val imagePreprocessor = GaussianGradientImagePreprocessor(0.1f)
      // number of points should usually be an odd number, so that the profiles are centered on the profiled points
      val featureExtractor = NormalDirectionFeatureExtractor(numberOfPoints = 5, spacing = 1.0)
      def samplerPerMesh(mesh: TriangleMesh[_3D]): Sampler[_3D] = UniformMeshSampler3D(mesh, numberOfPoints = 1000)
      val searchMethod = NormalDirectionSearchPointSampler(numberOfPoints = 31, searchDistance = 6)
      val fittingConfig = FittingConfiguration(featureDistanceThreshold = 2.0, pointDistanceThreshold = 3.0, modelCoefficientBounds = 3.0)

      val shapeModel = StatismoIO.readStatismoMeshModel(new File(getClass.getResource(s"/asmData/model.h5").getPath)).get
      val nbFiles = 7
      // use iterators so files are only loaded when required (and memory can be reclaimed after use)
      val meshes = (0 until nbFiles).toIterator map (i => MeshIO.readMesh(new File(getClass.getResource(s"/asmData/$i.stl").getPath)).get)
      val images = (0 until nbFiles).toIterator map (i => ImageIO.read3DScalarImage[Float](new File(getClass.getResource(s"/asmData/$i.vtk").getPath)).get)

      val targetImage = images.next()
      val targetMesh = meshes.next()
      val trainMeshes = meshes
      val trainImages = images

      val dc = DataCollection.fromMeshSequence(shapeModel.referenceMesh, trainMeshes.toIndexedSeq)._1.get
      val trainingData = trainImages zip dc.dataItems.toIterator.map(_.transformation)

      val asm = ActiveShapeModel.trainModel(shapeModel, trainingData, imagePreprocessor, featureExtractor, samplerPerMesh)

      // align the model
      val alignment = LandmarkRegistration.rigid3DLandmarkRegistration((asm.statisticalModel.mean.pointSet.points zip targetMesh.pointSet.points).toIndexedSeq, Point(0, 0, 0))
      val alignedASM = asm.transform(alignment)

    }
    it("Can be built, transformed and correctly fitted from/to artificial data") {

      val fit = Fixture.alignedASM.fit(Fixture.targetImage, Fixture.searchMethod, 20, Fixture.fittingConfig).get.mesh
      assert(MeshMetrics.diceCoefficient(fit, Fixture.targetMesh) > 0.94)
    }

    it("Can be transformed correctly from within the fitting") {

      val nullInitialParameters = DenseVector.zeros[Double](Fixture.asm.statisticalModel.rank)
      val fit = Fixture.asm.fit(Fixture.targetImage, Fixture.searchMethod, 20, Fixture.fittingConfig, ModelTransformations(nullInitialParameters, Fixture.alignment)).get.mesh
      assert(MeshMetrics.diceCoefficient(fit, Fixture.targetMesh) > 0.95)
    }
  }

}