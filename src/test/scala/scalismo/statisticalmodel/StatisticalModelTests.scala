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
import java.net.URLDecoder

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import scalismo.ScalismoTestSuite
import scalismo.geometry._
import scalismo.io.StatismoIO
import scalismo.mesh.MeshMetrics
import scalismo.numerics.PivotedCholesky.NumberOfEigenfunctions
import scalismo.registration.{RigidTransformation, RigidTransformationSpace}
import scalismo.statisticalmodel.dataset.DataCollection
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
        inst.pointSet.points
          .zip(instNew.pointSet.points)
          .foreach {
            case (pt1, pt2) =>
              (pt1.toVector - pt2.toVector).norm should be(0.0 +- (0.1))
          }
      }
    }

    it("can be calculated with a small amount of samples using PCA") {

      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(URLDecoder.decode(path, "UTF-8"))).get

      val ref = model.referenceMesh

      val data = (1 to 3).map(f => model.sample())

      val dc = DataCollection.fromTriangleMeshSequence(ref, data)
      val dcGpa = DataCollection.gpa(dc)

      val pca1 = StatisticalMeshModel.createUsingPCA(dcGpa, NumberOfEigenfunctions.apply(data.length - 1))

      val pca2 = StatisticalMeshModel.createUsingPCA(dcGpa) // NotConvergedException

      assert(pca1.isSuccess && pca2.isSuccess)
    }

    it("can be transformed forth and back and yield the same deformations") {
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(URLDecoder.decode(path, "UTF-8"))).get

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val rigidTransform = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)
      val inverseTransform = rigidTransform.inverse.asInstanceOf[RigidTransformation[_3D]]
      val transformedModel = model.transform(rigidTransform)
      val newModel = transformedModel.transform(inverseTransform)
      compareModels(model, newModel)
    }

    it("can change the mean shape and still yield the same shape space") {

      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(URLDecoder.decode(path, "UTF-8"))).get

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
      val model = StatismoIO.readStatismoMeshModel(new File(URLDecoder.decode(path, "UTF-8"))).get

      val newRank = model.rank / 2
      val truncatedModel = model.truncate(newRank)
      truncatedModel.rank should equal(newRank)

    }

    it("yield equivalent samples with reduced number of points when decimated") {
      val path = getClass.getResource("/facemodel.h5").getPath
      val model = StatismoIO.readStatismoMeshModel(new File(URLDecoder.decode(path, "UTF-8"))).get
      val targetNumberOfPoints = model.referenceMesh.pointSet.numberOfPoints / 2
      val decimatedModel = model.decimate(targetNumberOfPoints)

      val randomMesh = model.sample()
      val coeffs = model.coefficients(randomMesh)

      val correspondingSampleDecimatedModel = decimatedModel.instance(coeffs)
      decimatedModel.referenceMesh.pointSet.numberOfPoints should be < (model.referenceMesh.pointSet.numberOfPoints)
      correspondingSampleDecimatedModel.pointSet.numberOfPoints should equal(
        decimatedModel.referenceMesh.pointSet.numberOfPoints
      )

      MeshMetrics.hausdorffDistance(randomMesh, correspondingSampleDecimatedModel) < 1
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
