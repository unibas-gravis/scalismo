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

import org.scalatest.{ Matchers, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import java.io.{ PrintWriter, FileOutputStream, File }
import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.common.{ ScalarArray, SpatiallyIndexedDiscreteDomain, VectorField }
import scalismo.geometry._3D
import scalismo.image.{ DiscreteScalarImage, DifferentiableScalarImage, ScalarImage }
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.statisticalmodel.ActiveShapeModel.FeatureExtractor.NormalDirectionFeatureExtractor
import scalismo.statisticalmodel.{ MultivariateNormalDistribution, ActiveShapeModel }
import scalismo.statisticalmodel.ActiveShapeModel.{ ProfileDistributions }
import scala.util.{ Try, Success }
import ncsa.hdf.`object`.Group

/**
 * Created by Luethi on 09.03.14.
 */
class ActiveShapeModelIOTests extends FunSpec with Matchers {

  scalismo.initialize()

  private def createTmpH5File(): File = {
    val f = File.createTempFile("hdf5file", ".h5")
    f.deleteOnExit()
    f
  }

  private def createASM: ActiveShapeModel[NormalDirectionFeatureExtractor] = {
    val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
    val shapeModel = StatismoIO.readStatismoMeshModel(statismoFile).get

    val (profilePoints, _) = (new FixedPointsUniformMeshSampler3D(shapeModel.referenceMesh, 100, 42)).sample.unzip
    val ptDomain = SpatiallyIndexedDiscreteDomain.fromSeq(profilePoints)
    val dists = for (i <- 0 until ptDomain.numberOfPoints) yield (new MultivariateNormalDistribution(DenseVector.ones[Float](3) * i.toFloat, DenseMatrix.eye[Float](3) * i.toFloat))
    val profileDists = ProfileDistributions(ptDomain, dists)
    new ActiveShapeModel(shapeModel, profileDists, new NormalDirectionFeatureExtractor(5, 10))
  }

  describe("An active shape model") {

    it("can be written to disk and read again") {
      val originalASM = createASM

      val h5file = createTmpH5File()

      val statusWrite = for {
        _ <- ActiveShapeModelIO.writeASMOld(originalASM, h5file)
      } yield ()

      statusWrite.get // throw error if it occured

      // read it again
      val newAsmOrError = for {
        asm <- ActiveShapeModelIO.readASMOld[NormalDirectionFeatureExtractor](h5file)
      } yield asm

      val newASM = newAsmOrError.get
      newASM.profileDistributions should equal(originalASM.profileDistributions)

    }

    ignore("FIXME: remove") {

      val img = ImageIO.read3DScalarImage[Short](new File("/home/langguth/workspaces/stk.idea/bladderdemo/src/main/resources/volumes/22.vtk")).get
      def something(in: VectorField[_3D, _3D]): DiscreteScalarImage[_3D, Double] = {
        val vals = img.domain.points.map(p => in(p).norm).toArray
        DiscreteScalarImage[_3D, Double](img.domain, ScalarArray(vals))
      }

      val fe = NormalDirectionFeatureExtractor(7, 1.5)
      //      ImageIO.writeVTK(something(img.interpolate(1).differentiate), new File("/tmp/22-interpolated-1.vtk")).get
      //      ImageIO.writeVTK(something(img.interpolate(2).differentiate), new File("/tmp/22-interpolated-2.vtk")).get
      //      ImageIO.writeVTK(something(img.interpolate(3).differentiate), new File("/tmp/22-interpolated-3.vtk")).get
      val dimg = img.interpolate(1)
      val mesh = MeshIO.readMesh(new File("/tmp/bladdermean.vtk")).get
      val out = new PrintWriter(new FileOutputStream(new File("/tmp/scala.txt")))
      mesh.points foreach { pt =>
        val features = fe.apply(dimg, mesh, pt)
        out.println(s"$pt: $features");
      }
      out.flush()
      out.close()
    }

    it("FIXME: convert old-style to new style") {
      val asmIn = ActiveShapeModelIO.readASMOld[NormalDirectionFeatureExtractor](new File("/home/langguth/workspaces/stk.idea/bladderdemo/src/main/resources/asmModels/asmLevel-0.h5")).get
      val asmOut = ActiveShapeModel.bugfix(asmIn)
      val tmpFile = new File("/tmp/shinynewasm.h5")
      ActiveShapeModelIO.writeActiveShapeModel(asmOut, tmpFile).get
      val asmIn2 = ActiveShapeModelIO.readActiveShapeModel[NormalDirectionFeatureExtractor](tmpFile).get
      asmIn2 should equal(asmOut)
    }

  }

}
