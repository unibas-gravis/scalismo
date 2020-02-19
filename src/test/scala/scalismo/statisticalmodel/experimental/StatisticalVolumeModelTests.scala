package scalismo.statisticalmodel.experimental

import java.io.File
import java.net.URLDecoder

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import scalismo.ScalismoTestSuite
import scalismo.geometry.{_3D, Point}
import scalismo.io.StatismoIO
import scalismo.registration.{RigidTransformation, RigidTransformationSpace}
import scalismo.utils.Random

class StatisticalVolumeModelTests extends ScalismoTestSuite {

  implicit val random = Random(42)

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A statistical Volume mesh model") {

    def compareModels(oldModel: StatisticalVolumeMeshModel, newModel: StatisticalVolumeMeshModel) {

      for (i <- 0 until 10) {
        val standardNormal = Gaussian(0, 1)(random.breezeRandBasis)
        val coeffsData = standardNormal.sample(oldModel.rank)
        val coeffs = DenseVector(coeffsData.toArray)
        val inst = oldModel.instance(coeffs)
        val instNew = newModel.instance(coeffs)
        inst.pointSet.points
          .zip(instNew.pointSet.points)
          .foreach {
            case (pt1, pt2) =>
              (pt1.toVector - pt2.toVector).norm should be(0.0 +- (0.1))
          }
      }
    }

    it("can be transformed forth and back and yield the same deformations") {
      val path = getClass.getResource("/TetraMeshModel2.h5").getPath
      val model = StatismoIO.readStatismoVolumeMeshModel(new File(URLDecoder.decode(path))).get

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val rigidTransform = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)
      val inverseTransform = rigidTransform.inverse.asInstanceOf[RigidTransformation[_3D]]
      val transformedModel = model.transform(rigidTransform)
      val newModel = transformedModel.transform(inverseTransform)
      compareModels(model, newModel)
    }

    it("can change the mean shape and still yield the same shape space") {

      val path = getClass.getResource("/TetraMeshModel2.h5").getPath
      val model = StatismoIO.readStatismoVolumeMeshModel(new File(URLDecoder.decode(path))).get

      val newMesh = model.sample

      def t(pt: Point[_3D]): Point[_3D] = {
        val ptId = model.referenceVolumeMesh.pointSet.findClosestPoint(pt).id
        newMesh.pointSet.point(ptId)
      }

      val newModel = model.changeReference(t)

      compareModels(model, newModel)
    }

  }
}
