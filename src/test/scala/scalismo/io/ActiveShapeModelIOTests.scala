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
package scalismo.io

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.ScalismoTestSuite
import scalismo.common.SpatiallyIndexedDiscreteDomain
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.statisticalmodel.asm.{ActiveShapeModel, NormalDirectionGradientGaussianFeatureExtractor, Profiles}

import scala.collection.immutable

class ActiveShapeModelIOTests extends ScalismoTestSuite {

  private def createTmpH5File(): File = {
    val f = File.createTempFile("hdf5file", ".h5")
    f.deleteOnExit()
    f
  }

  private def createAsm(): ActiveShapeModel = {
    val statismoFile = new File(getClass.getResource("/facemodel.h5").getPath)
    val shapeModel = StatismoIO.readStatismoMeshModel(statismoFile).get

    val (sprofilePoints, _) = new FixedPointsUniformMeshSampler3D(shapeModel.referenceMesh, 100, 42).sample.unzip
    val (profilePoints, pointIds) = sprofilePoints.map { point => shapeModel.referenceMesh.findClosestPoint(point) }.unzip
    val ptDomain = SpatiallyIndexedDiscreteDomain.fromSeq(profilePoints)
    val dists = for (i <- 0 until ptDomain.numberOfPoints) yield new MultivariateNormalDistribution(DenseVector.ones[Float](3) * i.toFloat, DenseMatrix.eye[Float](3) * i.toFloat)
    val profiles = Profiles(ptDomain, dists)
    new ActiveShapeModel(shapeModel, profiles, NormalDirectionGradientGaussianFeatureExtractor(5, 10, 0), pointIds.to[immutable.IndexedSeq])
  }

  describe("An active shape model") {

    it("can be written to disk and read again") {
      val originalAsm = createAsm()
      val h5file = createTmpH5File()

      ActiveShapeModelIO.writeActiveShapeModel(originalAsm, h5file).get
      val newAsm = ActiveShapeModelIO.readActiveShapeModel(h5file).get

      newAsm should equal(originalAsm)
      h5file.delete()
    }
  }

}
