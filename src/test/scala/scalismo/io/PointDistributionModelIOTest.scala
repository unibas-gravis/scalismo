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
import scalismo.ScalismoTestSuite
import scalismo.geometry._
import scalismo.common.{DiscreteDomain, PointId, UnstructuredPointsDomain}
import scalismo.common.UnstructuredPoints.Create.{
  CreateUnstructuredPoints1D,
  CreateUnstructuredPoints2D,
  CreateUnstructuredPoints3D
}
import scalismo.common.UnstructuredPointsDomain.Create.{
  CreateUnstructuredPointsDomain1D,
  CreateUnstructuredPointsDomain2D,
  CreateUnstructuredPointsDomain3D
}
import scalismo.common.interpolation.NearestNeighborInterpolator
import scalismo.io.PointDistributionModelIOTest.DummySampler
import scalismo.kernels.{DiagonalKernel, GaussianKernel}
import scalismo.mesh.{
  LineCell,
  LineList,
  LineMesh,
  LineMesh2D,
  TriangleCell,
  TriangleList,
  TriangleMesh,
  TriangleMesh3D
}
import scalismo.numerics.Sampler
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, PointDistributionModel}
import scalismo.utils.Random

import scala.language.higherKinds

class PointDistributionModelIOTest extends ScalismoTestSuite {

  implicit val rng: Random = scalismo.utils.Random(42L)

  describe("a Point Distribution Model") {

    def assertModelAlmostEqual[D: NDSpace, DDomain[D] <: DiscreteDomain[D]](
      model1: PointDistributionModel[D, DDomain],
      model2: PointDistributionModel[D, DDomain]
    ): Unit = {

      assert(model1.mean == model2.mean)
      assert(breeze.linalg.norm(model1.gp.variance - model2.gp.variance) < 1e-5)
      assert(breeze.linalg.sum(model1.gp.basisMatrix - model2.gp.basisMatrix) < 1e-5)
    }

    def create3Dmodel(): PointDistributionModel[_3D, TriangleMesh] = {
      val gk = DiagonalKernel(GaussianKernel[_3D](10.0), 3)
      val gp = GaussianProcess[_3D, EuclideanVector[_3D]](gk)
      val pointSet = CreateUnstructuredPoints3D.create(
        IndexedSeq(Point3D(0, 0, 0), Point3D(1, 0, 0), Point3D(1, 1, 0), Point3D(0, 1, 0))
      )
      val topology = TriangleList(
        IndexedSeq(TriangleCell(PointId(0), PointId(1), PointId(2)), TriangleCell(PointId(1), PointId(2), PointId(3)))
      )
      val reference = TriangleMesh3D(pointSet, topology)
      val sampler = DummySampler(reference)
      val gpLwRnk = LowRankGaussianProcess.approximateGPNystrom(gp, sampler)
      PointDistributionModel(reference, gpLwRnk)
    }

    def create2Dmodel(): PointDistributionModel[_2D, LineMesh] = {
      val gk = DiagonalKernel(GaussianKernel[_2D](10.0), 2)
      val gp = GaussianProcess[_2D, EuclideanVector[_2D]](gk)
      val pointSet = CreateUnstructuredPoints2D.create(IndexedSeq(Point2D(0, 0), Point2D(1, 0), Point2D(1, 1)))
      val topology = LineList(
        IndexedSeq(LineCell(PointId(0), PointId(1)), LineCell(PointId(1), PointId(2)), LineCell(PointId(2), PointId(0)))
      )
      val reference = LineMesh2D(pointSet, topology)
      val sampler = DummySampler(reference)
      val gpLwRnk = LowRankGaussianProcess.approximateGPNystrom(gp, sampler)
      PointDistributionModel(reference, gpLwRnk)
    }

    def create1Dmodel(): PointDistributionModel[_1D, UnstructuredPointsDomain] = {
      val gk = DiagonalKernel(GaussianKernel[_1D](10.0), 1)
      val gp = GaussianProcess[_1D, EuclideanVector[_1D]](gk)
      val pointSetDomain = CreateUnstructuredPointsDomain1D.create(IndexedSeq(Point1D(0), Point1D(1), Point1D(3)))
      val sampler = DummySampler(pointSetDomain)
      val gpLwRnk = LowRankGaussianProcess.approximateGPNystrom(gp, sampler)
      PointDistributionModel(pointSetDomain, gpLwRnk)
    }

    val model3D: PointDistributionModel[_3D, TriangleMesh] = create3Dmodel()
    val model2D: PointDistributionModel[_2D, LineMesh] = create2Dmodel()
    val model1D: PointDistributionModel[_1D, UnstructuredPointsDomain] = create1Dmodel()

    it("can write and read a 3D PDM with a TriangleMesh domain") {
      val dummyFile = new java.io.File("./x.h5.json") //File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalTriangleMeshModel3D(model3D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalTriangleMeshModel3D(dummyFile)
      } yield {
        assertModelAlmostEqual[_3D, TriangleMesh](model3D, readModel)
      }
      t.get
    }

    it("can read a 3D TriangleMeshModel as a PDM without cell connectivity") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalTriangleMeshModel3D(model3D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalPointModel3D(dummyFile)
      } yield {
        assert(readModel.reference.pointSet == model3D.reference.pointSet)
      }
      t.get
    }

    it("can write and read a 2D PDM with a LineMesh domain") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalLineMeshModel2D(model2D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalLineMeshModel2D(dummyFile)
      } yield {
        assertModelAlmostEqual[_2D, LineMesh](model2D, readModel)
      }
      t.get
    }

    it("can read a 2D TriangleMeshModel as a PDM without cell connectivity") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalLineMeshModel2D(model2D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalPointModel2D(dummyFile)
      } yield {
        assert(readModel.reference.pointSet == model2D.reference.pointSet)
      }
      t.get
    }

    it("can write and read a 3D PDM") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()
      val model3Dpdm: PointDistributionModel[_3D, UnstructuredPointsDomain] =
        model3D.newReference[UnstructuredPointsDomain](
          CreateUnstructuredPointsDomain3D.create(model3D.reference.pointSet.points.toIndexedSeq),
          NearestNeighborInterpolator()
        )

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalPointModel3D(model3Dpdm, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalPointModel3D(dummyFile)
      } yield {
        assertModelAlmostEqual[_3D, UnstructuredPointsDomain](model3Dpdm, readModel)
      }
      t.get
    }

    it("can write and read a 2D PDM") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()
      val model2Dpdm: PointDistributionModel[_2D, UnstructuredPointsDomain] =
        model2D.newReference[UnstructuredPointsDomain](
          CreateUnstructuredPointsDomain2D.create(model2D.reference.pointSet.points.toIndexedSeq),
          NearestNeighborInterpolator()
        )
      val t = for {
        _ <- StatisticalModelIO.writeStatisticalPointModel2D(model2Dpdm, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalPointModel2D(dummyFile)
      } yield {
        assertModelAlmostEqual[_2D, UnstructuredPointsDomain](model2Dpdm, readModel)
      }
      t.get
    }

    it("can write and read a 1D PDM") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalPointModel1D(model1D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalPointModel1D(dummyFile)
      } yield {
        assertModelAlmostEqual[_1D, UnstructuredPointsDomain](model1D, readModel)
      }
      t.get
    }

    it("fails on reading a 3D TriangleMesh when specifying wrong dimensionality") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalTriangleMeshModel3D(model3D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalTriangleMeshModel2D(dummyFile)
      } yield {
        readModel
      }
      t.isFailure should be(true)
    }

    it("fails on reading a 3D TriangleMesh when specifying wrong domain cell type") {
      val dummyFile = File.createTempFile("dummy", "h5.json")
      dummyFile.deleteOnExit()

      val t = for {
        _ <- StatisticalModelIO.writeStatisticalTriangleMeshModel3D(model3D, dummyFile)
        readModel <- StatisticalModelIO.readStatisticalLineMeshModel3D(dummyFile)
      } yield {
        readModel
      }
      t.isFailure should be(true)
    }

  }
}

object PointDistributionModelIOTest {
  case class DummySampler[D: NDSpace](domain: DiscreteDomain[D]) extends Sampler[D] {
    override def volumeOfSampleRegion: Double = domain.pointSet.boundingBox.volume

    override val numberOfPoints: Int = domain.pointSet.numberOfPoints

    private val p = 1.0 / volumeOfSampleRegion

    override def sample(): IndexedSeq[(Point[D], Double)] = {
      domain.pointSet.points.toIndexedSeq.map(pt => (pt, p))
    }
  }

}
