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

import scalismo.geometry._
import scalismo.io.StatismoIO
import scalismo.registration.{ RigidTransformation, RigidTransformationSpace }

import scala.language.implicitConversions
import breeze.linalg.{ DenseVector, DenseMatrix }
import java.io.File
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import breeze.stats.distributions.RandBasis
import org.apache.commons.math3.random.MersenneTwister
import scalismo.io.MeshIO
class StatisticalModelTests extends FunSpec with Matchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  scalismo.initialize()

  describe("A statistical model") {

    def compareModels(oldModel: StatisticalMeshModel, newModel: StatisticalMeshModel) {

      for (i <- 0 until 10) {
        val coeffsData = (0 until oldModel.rank).map { _ =>
          breeze.stats.distributions.Gaussian(0, 1).draw().toFloat
        }
        val coeffs = DenseVector(coeffsData.toArray)
        val inst = oldModel.instance(coeffs)
        val instNew = newModel.instance(coeffs)
        inst.points.zip(instNew.points)
          .foreach {
            case (pt1, pt2) =>
              (pt1.toVector - pt2.toVector).norm should be(0.0 +- (0.1))
          }
      }
    }

    it("can be transformed forth and back and yield the same deformations") {
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val rigidTransform = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)
      val inverseTransform = rigidTransform.inverse.asInstanceOf[RigidTransformation[_3D]]
      val transformedModel = model.transform(rigidTransform)
      val newModel = transformedModel.transform(inverseTransform)
      compareModels(model, newModel)
    }

    it("can change the mean shape and still yield the same shape space") {
      scalismo.initialize()
      val path = getClass().getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(path)).get

      val newMesh = model.sample

      def t(pt: Point[_3D]): Point[_3D] = {
        val (refPt, ptId) = model.referenceMesh.findClosestPoint(pt)
        newMesh(ptId)
      }

      val newModel = model.changeReference(t)

      compareModels(model, newModel)
    }

    //    it("can write a changed mean statistical mode, read it and still yield the same space") {
    //      val tmpStatismoFile = File.createTempFile("statModel", ".h5")
    //      tmpStatismoFile.deleteOnExit()
    //
    //      StatismoIO.writeStatismoMeshModel(newModel, tmpStatismoFile)
    //      val readModel = StatismoIO.readStatismoMeshModel(tmpStatismoFile).get
    //      compareModels(model, readModel)
    //    }

  }

}