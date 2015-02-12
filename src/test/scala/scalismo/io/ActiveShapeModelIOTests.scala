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

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.SpatiallyIndexedFiniteDiscreteDomain
import scalismo.image.ScalarImage
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.statisticalmodel.{ASMProfileDistributions, MultivariateNormalDistribution, ActiveShapeModel}
import scalismo.statisticalmodel.ActiveShapeModel.NormalDirectionFeatureExtractor
import scala.util.{Try, Success}
import ncsa.hdf.`object`.Group

/**
 * Created by Luethi on 09.03.14.
 */
class ActiveShapeModelIOTests  extends FunSpec with Matchers {

  scalismo.initialize()

  private def createTmpH5File() : File = {
    val f= File.createTempFile("hdf5file", ".h5")
    f.deleteOnExit()
    f
  }



  private def createASM : ActiveShapeModel[NormalDirectionFeatureExtractor] = {
    val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
    val shapeModel = StatismoIO.readStatismoMeshModel(statismoFile).get

    val (profilePoints, _) = (new FixedPointsUniformMeshSampler3D(shapeModel.referenceMesh, 100, 42)).sample.unzip
    val ptDomain = SpatiallyIndexedFiniteDiscreteDomain.fromSeq(profilePoints)
    val dists = for (i <- 0 until ptDomain.numberOfPoints) yield
      (new MultivariateNormalDistribution(DenseVector.ones[Float](3) * i.toFloat , DenseMatrix.eye[Float](3) * i.toFloat))
    val profileDists = ASMProfileDistributions(ptDomain, dists)
    new ActiveShapeModel(shapeModel,  profileDists, new NormalDirectionFeatureExtractor(5, 10))
  }

  describe("An active shape model") {

    it("can be written to disk and read again") {
      val originalASM = createASM

      val h5file = createTmpH5File()

      val statusWrite = for {
        _ <- ActiveShapeModelIO.writeASM(originalASM, h5file)
      } yield ()

      statusWrite.get // throw error if it occured

      // read it again
      val newAsmOrError = for {
        asm <- ActiveShapeModelIO.readASM[NormalDirectionFeatureExtractor](h5file)
      } yield asm

      val newASM = newAsmOrError.get
      newASM.intensityDistributions should equal(originalASM.intensityDistributions)

    }

  }

}
