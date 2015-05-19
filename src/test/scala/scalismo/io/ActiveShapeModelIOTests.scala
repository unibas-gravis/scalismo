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
import org.scalatest.{FunSpec, Matchers}
import scalismo.common.SpatiallyIndexedDiscreteDomain
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.statisticalmodel.asm.{ActiveShapeModel, NormalDirectionGradientGaussianFeatureExtractor, Profiles}

import scala.collection.immutable

class ActiveShapeModelIOTests extends FunSpec with Matchers {

  scalismo.initialize()

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
    //
    //    ignore("FIXME: remove") {
    //
    //      val img = ImageIO.read3DScalarImage[Short](new File("/home/langguth/workspaces/stk.idea/bladderdemo/src/main/resources/volumes/22.vtk")).get
    //      def something(in: VectorField[_3D, _3D]): DiscreteScalarImage[_3D, Double] = {
    //        val vals = img.domain.points.map(p => in(p).norm).toArray
    //        DiscreteScalarImage[_3D, Double](img.domain, ScalarArray(vals))
    //      }
    //
    //      val fe = NormalDirectionFeatureExtractor(7, 1.5f, 0)
    //      //      ImageIO.writeVTK(something(img.interpolate(1).differentiate), new File("/tmp/22-interpolated-1.vtk")).get
    //      //      ImageIO.writeVTK(something(img.interpolate(2).differentiate), new File("/tmp/22-interpolated-2.vtk")).get
    //      //      ImageIO.writeVTK(something(img.interpolate(3).differentiate), new File("/tmp/22-interpolated-3.vtk")).get
    //      val dimg = img.interpolate(1)
    //      val mesh = MeshIO.readMesh(new File("/tmp/bladdermean.vtk")).get
    //      val out = new PrintWriter(new FileOutputStream(new File("/tmp/scala.txt")))
    //      val fei = fe.getInstance(img.map {_.toFloat})
    //      mesh.points foreach { pt =>
    //        val features = fei.extractFeatures(mesh, pt)
    //        out.println(s"$pt: $features");
    //      }
    //      out.flush()
    //      out.close()
    //    }
    //
    //    it("FIXME: convert old-style to new style") {
    //      val asmIn = ActiveShapeModelIO.readASMOld[NormalDirectionFeatureExtractor](new File("/home/langguth/workspaces/stk.idea/bladderdemo/src/main/resources/asmModels/asmLevel-0.h5")).get
    //      val asmOut = ActiveShapeModel.bugfix(asmIn)
    //      val tmpFile = new File("/tmp/shinynewasm.h5")
    //      ActiveShapeModelIO.writeActiveShapeModel(asmOut, tmpFile).get
    //      val asmIn2 = ActiveShapeModelIO.readActiveShapeModel[NormalDirectionFeatureExtractor](tmpFile).get
    //      asmIn2 should equal(asmOut)
    //    }
    //
  }

}
