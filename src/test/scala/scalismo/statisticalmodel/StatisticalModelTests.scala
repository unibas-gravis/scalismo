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
package scalismo.statisticalmodel

import java.io.File

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import scalismo.ScalismoTestSuite
import scalismo.geometry._
import scalismo.io.StatismoIO
import scalismo.registration._
import scalismo.utils.Random

import scala.language.implicitConversions
class StatisticalModelTests extends ScalismoTestSuite {

  implicit val random = Random(42)

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A statistical model") {

    def compareModels(oldModel: StatisticalMeshModel, newModel: StatisticalMeshModel) {

      for (i <- 0 until 10) {
        val standardNormal = Gaussian(0, 1)(random.breezeRandBasis)
        val coeffsData = standardNormal.sample(oldModel.rank)
        val coeffs = DenseVector(coeffsData.toArray)
        val inst = oldModel.instance(coeffs)
        val instNew = newModel.instance(coeffs)
        inst.pointSet.points.zip(instNew.pointSet.points)
          .foreach {
            case (pt1, pt2) =>
              (pt1.toVector - pt2.toVector).norm should be(0.0 +- (0.1))
          }
      }
    }

    it("can be transformed forth and back and yield the same deformations") {
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val rigidTransform = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)
      val inverseTransform = rigidTransform.inverse.asInstanceOf[RigidTransformation[_3D]]
      val transformedModel = model.transform(rigidTransform)
      val newModel = transformedModel.transform(inverseTransform)
      compareModels(model, newModel)
    }

    it("can change the mean shape and still yield the same shape space") {

      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val newMesh = model.sample

      def t(pt: Point[_3D]): Point[_3D] = {
        val ptId = model.referenceMesh.pointSet.findClosestPoint(pt).id
        newMesh.pointSet.point(ptId)
      }

      val newModel = model.changeReference(t)

      compareModels(model, newModel)
    }

    it("has the right rank when reduced") {
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val newRank = model.rank / 2
      val truncatedModel = model.truncate(newRank)
      truncatedModel.rank should equal(newRank)
    }

    //    it("can write a changed mean statistical mode, read it and still yield the same space") {
    //      val tmpStatismoFile = File.createTempFile("statModel", ".h5")
    //      tmpStatismoFile.deleteOnExit()
    //
    //      StatismoIO.writeStatismoMeshModel(newModel, tmpStatismoFile)
    //      val readModel = StatismoIO.readStatismoMeshModel(tmpStatismoFile).get
    //      compareModels(model, readModel)
    //    }

    it("can perform similarity transform back and forth on statistical model") {
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val rigidTransform = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)
      val scalingTranform = ScalingTransformation[_3D](2)
      val similarityTranform = SimilarityTransformation(scalingTranform, rigidTransform)
      val inverseTransform = similarityTranform.inverse
      val transformedModel = model.transform(similarityTranform)
      val newModel = transformedModel.transform(inverseTransform)
      compareModels(model, newModel)
    }

  }

}