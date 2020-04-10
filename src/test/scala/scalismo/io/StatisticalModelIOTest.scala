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
import java.net.URLDecoder

import scalismo.ScalismoTestSuite
import scalismo.common.NearestNeighborInterpolator
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain}
import scalismo.kernels.{DiagonalKernel, GaussianKernel}
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel}

class StatisticalModelIOTest extends ScalismoTestSuite {

  implicit val rng = scalismo.utils.Random(42L)

  describe("a Statismo Mesh Model") {

    def assertModelAlmostEqual(model1: StatisticalMeshModel, model2: StatisticalMeshModel): Unit = {
      assert(model1.mean == model2.mean)
      assert(breeze.linalg.norm(model1.gp.variance - model2.gp.variance) < 1e-5)
      assert(breeze.linalg.sum(model1.gp.basisMatrix - model2.gp.basisMatrix) < 1e-5)
    }

    it("can be written and read again") {
      val statismoFile = new File(URLDecoder.decode(getClass.getResource("/facemodel.h5").getPath, "UTF-8"))
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit()

      val t = for {
        model <- StatisticalModelIO.readStatisticalMeshModel(statismoFile)
        _ <- StatisticalModelIO.writeStatisticalMeshModel(model, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalMeshModel(dummyFile)
      } yield {
        assertModelAlmostEqual(model, readModel)
      }
      t.get

    }

    it("can be written and read again in non-standard location") {
      val statismoFile = new File(URLDecoder.decode(getClass.getResource("/facemodel.h5").getPath, "UTF-8"))
      val dummyFile = File.createTempFile("dummy", "h5")
      dummyFile.deleteOnExit()

      val t = for {
        model <- StatisticalModelIO.readStatisticalMeshModel(statismoFile)
        _ <- StatisticalModelIO.writeStatisticalMeshModel(model, dummyFile, "/someLocation")
        readModel <- StatisticalModelIO.readStatisticalMeshModel(dummyFile, "/someLocation")
      } yield {
        assertModelAlmostEqual(model, readModel)
      }
      t.get

    }

    it("model in version 0.81 can be read") {
      val statismoFile = new File(URLDecoder.decode(getClass.getResource("/facemodel.h5").getPath, "UTF-8"))
      val statismoOldFile = new File(URLDecoder.decode(getClass.getResource("/facemodel_v081.h5").getPath, "UTF-8"))

      val t = for {
        model <- StatisticalModelIO.readStatisticalMeshModel(statismoFile)
        modelOld <- StatisticalModelIO.readStatisticalMeshModel(statismoOldFile)
      } yield {
        assertModelAlmostEqual(model, modelOld)
      }
      t.get

    }
  }

  it("can read a catalog") {
    val statismoFile = new File(URLDecoder.decode(getClass.getResource("/facemodel.h5").getPath, "UTF-8"))
    val catalog = StatismoIO.readModelCatalog(statismoFile).get
    catalog.size should equal(1)
    val firstEntry = catalog.head
    firstEntry.name should equal("faceshapemodel")
    firstEntry.modelType should equal(StatismoIO.StatismoModelType.Polygon_Mesh)
    firstEntry.modelPath should equal("/")
  }

  describe("a deformation model") {
    it("can be created, saved and reread in 3D") {
      val gk = DiagonalKernel(GaussianKernel[_3D](10.0), 3)
      val gp = GaussianProcess[_3D, EuclideanVector[_3D]](gk)
      val domain = DiscreteImageDomain(origin = Point3D(1.0, 3.1, 7.5),
                                       spacing = EuclideanVector3D(0.8, 0.7, 0.6),
                                       size = IntVector3D(10, 12, 9))

      val lowrankGp = LowRankGaussianProcess.approximateGPCholesky(domain, gp, 0.1, NearestNeighborInterpolator())

      val tmpFile = java.io.File.createTempFile("adeformationfield", ".h5")
      tmpFile.deleteOnExit()
      val discreteGP = lowrankGp.discretize(domain)
      StatisticalModelIO.writeDeformationModel3D(discreteGP, tmpFile).get

      val discreteGPReread = StatisticalModelIO.readDeformationModel3D(tmpFile).get

      // origin and spacing are saved as float. Hence we expect small inaccuracies.
      (discreteGP.domain.origin - discreteGPReread.domain.origin).norm should be < 1e-5
      (discreteGP.domain.spacing - discreteGPReread.domain.spacing).norm should be < 1e-5
      discreteGP.domain.size should equal(discreteGPReread.domain.size)

      // also here, due to conversion in float, smaller errors are expected
      breeze.linalg.norm(discreteGP.meanVector - discreteGPReread.meanVector) should be < 1e-2
      breeze.linalg.sum(discreteGP.basisMatrix - discreteGPReread.basisMatrix) should be < 1e-2
      breeze.linalg.norm(discreteGP.variance - discreteGPReread.variance) should be < 1e-2

    }

    it("can be created, saved and reread in 2D") {
      val gk = DiagonalKernel(GaussianKernel[_2D](10.0), 2)
      val gp = GaussianProcess[_2D, EuclideanVector[_2D]](gk)
      val domain = DiscreteImageDomain(origin = Point2D(1.0, 3.1),
                                       spacing = EuclideanVector2D(0.8, 0.7),
                                       size = IntVector2D(10, 12))

      val lowrankGp = LowRankGaussianProcess.approximateGPCholesky(domain, gp, 0.1, NearestNeighborInterpolator())

      val tmpFile = java.io.File.createTempFile("adeformationfield", ".h5")
      tmpFile.deleteOnExit()
      val discreteGP = lowrankGp.discretize(domain)
      StatisticalModelIO.writeDeformationModel2D(discreteGP, tmpFile).get

      val discreteGPReread = StatisticalModelIO.readDeformationModel2D(tmpFile).get

      // origin and spacing are saved as float. Hence we expect small inaccuracies.
      (discreteGP.domain.origin - discreteGPReread.domain.origin).norm should be < 1e-5
      (discreteGP.domain.spacing - discreteGPReread.domain.spacing).norm should be < 1e-5
      discreteGP.domain.size should equal(discreteGPReread.domain.size)

      // also here, due to conversion in float, smaller errors are expected
      breeze.linalg.norm(discreteGP.meanVector - discreteGPReread.meanVector) should be < 1e-2
      breeze.linalg.sum(discreteGP.basisMatrix - discreteGPReread.basisMatrix) should be < 1e-2
      breeze.linalg.norm(discreteGP.variance - discreteGPReread.variance) should be < 1e-2

    }

  }
}
