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

import scalismo.ScalismoTestSuite
import scalismo.common.DiscreteField.{ScalarMeshField, ScalarVolumeMeshField}
import scalismo.common.*
import scalismo.geometry.{_3D, Point}
import scalismo.io.MeshIOTests.{createRandomScalarVolumeMeshField, createRandomTetrahedralMesh}
import scalismo.mesh.*
import scalismo.utils.Random

import java.io.File
import java.net.URLDecoder
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class MeshIOTests extends ScalismoTestSuite {

  describe("MeshIO") {

    it("yields the original mesh when reading and writing") {
      val path = getClass.getResource("/facemesh.stl").getPath
      val origMesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get

      def testWriteRead(extension: String): Unit = {
        val tmpFile = File.createTempFile("mesh", extension)
        val writeStatus = MeshIO.writeMesh(origMesh, tmpFile)
        writeStatus.isSuccess should be(true)

        val meshTry = MeshIO.readMesh(tmpFile)
        meshTry.isSuccess should be(true)
        meshTry.map { mesh =>
          mesh should equal(origMesh)
        }
      }
      testWriteRead(".stl")
      testWriteRead(".ply")
    }

    it("stl and ply readers (binary and ascii) yield the same result") {
      val pathSTL = getClass.getResource("/unit-sphere.stl").getPath
      val pathSTLascii = getClass.getResource("/unit-sphere_ascii.stl").getPath
      val pathPLY = getClass.getResource("/unit-sphere.ply").getPath
      val pathPLYascii = getClass.getResource("/unit-sphere_ascii.ply").getPath
      val meshSTL = MeshIO.readMesh(new File(URLDecoder.decode(pathSTL, "UTF-8"))).get
      val meshSTLascii = MeshIO.readMesh(new File(URLDecoder.decode(pathSTLascii, "UTF-8"))).get
      val meshPLY = MeshIO.readMesh(new File(URLDecoder.decode(pathPLY, "UTF-8"))).get
      val meshPLYascii = MeshIO.readMesh(new File(URLDecoder.decode(pathPLYascii, "UTF-8"))).get

      def compareMeshes(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]): Unit = {
        m1.triangulation.triangles.toSet should equal(m2.triangulation.triangles.toSet)
        m1.pointSet.points.toSeq.zip(m2.pointSet.points.toSeq).foreach { case (p1, p2) =>
          p1.x should be(p2.x +- 1e-6)
          p1.y should be(p2.y +- 1e-6)
          p1.z should be(p2.z +- 1e-6)
        }
      }

      compareMeshes(meshSTL, meshSTLascii)
      compareMeshes(meshSTL, meshPLY)
      compareMeshes(meshSTL, meshPLYascii)
    }
    //    it("yields the original polyline when reading  and writing a polyLine in 2D") {
    //      val path = getClass.getResource("/linemesh.vtk").getPath
    //      val origMesh = MeshIO.readLineMesh2D(new File(URLDecoder.decode(path, "UTF-8"))).get
    //
    //      val tmpFile = File.createTempFile("mesh", ".vtk")
    //      val writeStatus = MeshIO.writeLineMesh(origMesh, tmpFile)
    //      writeStatus.isSuccess should be(true)
    //
    //      val meshTry = MeshIO.readLineMesh2D(tmpFile)
    //      meshTry.isSuccess should be(true)
    //      meshTry.map { mesh =>
    //        mesh should equal(origMesh)
    //      }
    //
    //    }

    it("yields the original mesh when reading and writing a shape only ply") {
      val path = getClass.getResource("/mean_shapeOnly.ply").getPath
      val shape = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
      val tmpFile = File.createTempFile("mesh", ".ply")
      MeshIO.writeMesh(shape, tmpFile)
      val reRead = MeshIO.readMesh(tmpFile).get
      tmpFile.delete()

      reRead should equal(shape)
    }

    it("does not throw an exception when called with an nonexistant file") {
      noException should be thrownBy MeshIO.readMesh(new java.io.File("idonotexist.ply"))
    }

    it("yields the original mesh when reading and writing a vertex color ply") {
      val path = getClass.getResource("/mean_vertexColor.ply").getPath
      val shape = MeshIO.readVertexColorMesh3D(new File(URLDecoder.decode(path, "UTF-8"))).get
      val tmpFile = File.createTempFile("mesh", ".ply")
      MeshIO.writeVertexColorMesh3D(shape, tmpFile)
      val reRead = MeshIO.readVertexColorMesh3D(tmpFile).get
      tmpFile.delete()

      reRead should equal(shape)
    }

    it("correctly reads textured mesh and discards unsupported properties") {
      val pathTextured = getClass.getResource("/mean_textured.ply").getPath
      val pathVertexColor = getClass.getResource("/mean_vertexColor.ply").getPath
      val shapeTexture = MeshIO.readMesh(new File(URLDecoder.decode(pathTextured, "UTF-8"))).get
      val shapeVertexColor = MeshIO.readMesh(new File(URLDecoder.decode(pathVertexColor, "UTF-8"))).get
      shapeTexture should equal(shapeVertexColor)
    }

    //    it("correctly writes and reads a tetrahedral mesh to a vtk file ") {
    //      val temporaryFile = File.createTempFile("tetrahedralMesh", ".vtk")
    //      val originalMesh = createRandomTetrahedralMesh()
    //      MeshIO.writeTetrahedralMesh(originalMesh, temporaryFile)
    //      val loadingMesh = MeshIO.readTetrahedralMesh(temporaryFile)
    //      assert(loadingMesh.isSuccess)
    //      loadingMesh.map { loadedMesh =>
    //        assert(loadedMesh.pointSet == originalMesh.pointSet)
    //        assert(loadedMesh.tetrahedralization == originalMesh.tetrahedralization)
    //      }
    //    }

    //    it("correctly writes and reads a tetrahedral mesh to a vtu file ") {
    //      val temporaryFile = File.createTempFile("tetrahedralMesh", ".vtu")
    //      val originalMesh = createRandomTetrahedralMesh()
    //      MeshIO.writeTetrahedralMesh(originalMesh, temporaryFile)
    //      val loadingMesh = MeshIO.readTetrahedralMesh(temporaryFile)
    //      assert(loadingMesh.isSuccess)
    //      loadingMesh.map { loadedMesh =>
    //        assert(loadedMesh.pointSet == loadedMesh.pointSet)
    //        assert(loadedMesh.tetrahedralization == originalMesh.tetrahedralization)
    //      }
    //    }

    //    it("correctly writes and reads a scalar volume mesh field to a vtk file ") {
    //      val temporaryFile = File.createTempFile("scalarVolumeMeshField", ".vtk")
    //      val originalMesh = createRandomScalarVolumeMeshField()
    //      MeshIO.writeScalarVolumeMeshField(originalMesh, temporaryFile)
    //      val loadingMesh = MeshIO.readScalarVolumeMeshField[Int](temporaryFile)
    //      assert(loadingMesh.isSuccess)
    //      loadingMesh.map { loadedMesh =>
    //        assert(loadedMesh.domain.pointSet == originalMesh.domain.pointSet)
    //        assert(loadedMesh.domain.tetrahedralization == originalMesh.domain.tetrahedralization)
    //        assert(loadedMesh.data == originalMesh.data)
    //      }
    //    }

    //    it("correctly writes and reads a scalar volume mesh field to a vtu file ") {
    //      val temporaryFile = File.createTempFile("scalarVolumeMeshField", ".vtu")
    //      val originalMesh = createRandomScalarVolumeMeshField()
    //      MeshIO.writeScalarVolumeMeshField(originalMesh, temporaryFile)
    //      val loadingMesh = MeshIO.readScalarVolumeMeshField[Int](temporaryFile)
    //      assert(loadingMesh.isSuccess)
    //      loadingMesh.map { loadedMesh =>
    //        assert(loadedMesh.domain.pointSet == originalMesh.domain.pointSet)
    //        assert(loadedMesh.domain.tetrahedralization == originalMesh.domain.tetrahedralization)
    //        assert(loadedMesh.data == originalMesh.data)
    //      }
    //    }

  }

  //  describe("ScalarMeshField IO") {
  //
  //    object Fixture {
  //      val path: String = getClass.getResource("/facemesh.stl").getPath
  //      val mesh: TriangleMesh[_3D] = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
  //      val meshData: ScalarMeshField[Int] = ScalarMeshField(mesh, ScalarArray(mesh.pointSet.pointIds.map(_.id).toArray))
  //    }
  //
  //    def sameWriteRead[S: Scalar: ClassTag](): Try[ScalarMeshField[S]] = {
  //      val f = Fixture
  //      val tmpFile = File.createTempFile("scalarMesh", ".vtk")
  //
  //      val writeTry = MeshIO.writeScalarMeshField(f.meshData, tmpFile)
  //      assert(writeTry.isSuccess)
  //
  //      MeshIO.readScalarMeshField[S](tmpFile)
  //    }
  //
  //    it("can write and correctly read a ScalarMeshField") {
  //
  //      val f = Fixture
  //      val readTry = sameWriteRead[Int]()
  //      assert(readTry.isSuccess)
  //
  //      readTry.get.data == ScalarArray(f.mesh.pointSet.pointIds.map(_.id).toArray)
  //    }
  //
  //    it("fails when reading with the wrong Scalar type") {
  //      val readTry = sameWriteRead[Float]()
  //      assert(readTry.isFailure)
  //    }
  //
  //    it("can read and cast a ScalarMeshField") {
  //      val f = Fixture
  //      val tmpFile = File.createTempFile("scalarMesh", ".vtk")
  //      tmpFile.deleteOnExit()
  //      MeshIO.writeScalarMeshField(f.meshData, tmpFile)
  //
  //      MeshIO.readScalarMeshFieldAsType[Float](tmpFile) match {
  //        case Success(meshData) =>
  //          for ((origDatum, newDatum) <- f.meshData.data.zip(meshData.data)) {
  //            origDatum.toFloat should equal(newDatum)
  //          }
  //        case Failure(t) =>
  //          println(t.getMessage)
  //          t.printStackTrace()
  //          throw t
  //      }
  //
  //    }
  //
  //  }

  //  describe("ScalarVolumeMeshField IO") {
  //
  //    object Fixture {
  //      val meshData: ScalarVolumeMeshField[Int] = createRandomScalarVolumeMeshField()
  //      val mesh = meshData.domain
  //    }
  //
  //    def sameWriteRead[S: Scalar: ClassTag](): Try[ScalarVolumeMeshField[S]] = {
  //      val f = Fixture
  //      val tmpFile = File.createTempFile("scalarVolumeMeshField", ".vtk")
  //
  //      val writeTry = MeshIO.writeScalarVolumeMeshField(f.meshData, tmpFile)
  //      assert(writeTry.isSuccess)
  //
  //      MeshIO.readScalarVolumeMeshField[S](tmpFile)
  //    }
  //
  //    it("can write and correctly read a ScalarVolumeMeshField") {
  //      val f = Fixture
  //      val readTry = sameWriteRead[Int]()
  //      assert(readTry.isSuccess)
  //
  //      readTry.get.data == ScalarArray(f.mesh.pointSet.pointIds.map(_.id).toArray)
  //    }
  //
  //    it("fails when reading with the wrong Scalar type") {
  //      val readTry = sameWriteRead[Float]()
  //      assert(readTry.isFailure)
  //    }
  //
  //    it("can read and cast a ScalarVolumeMeshField") {
  //      val f = Fixture
  //      val tmpFile = File.createTempFile("scalarVolumeMeshField", ".vtk")
  //      tmpFile.deleteOnExit()
  //      MeshIO.writeScalarVolumeMeshField(f.meshData, tmpFile)
  //
  //      MeshIO.readScalarVolumeMeshFieldAsType[Float](tmpFile) match {
  //        case Success(meshData) =>
  //          for ((origDatum, newDatum) <- f.meshData.data.zip(meshData.data)) {
  //            origDatum.toInt should equal(newDatum)
  //          }
  //        case Failure(t) =>
  //          println(t.getMessage)
  //          t.printStackTrace()
  //          throw t
  //      }
  //
  //    }
  //
  //  }

}

object MeshIOTests {
  def createRandomTetrahedralMesh(): TetrahedralMesh3D = {
    // points around unit cube

    val rng = Random(42L)
    val N = 200
    val points = IndexedSeq.fill(N)(
      Point(rng.scalaRandom.nextGaussian() * 2,
            rng.scalaRandom.nextGaussian() * 100,
            rng.scalaRandom.nextGaussian() * 1000
      )
    )
    val domain = UnstructuredPoints(points)

    // cells covering the complete cube
    implicit def intToPointId(i: Int): PointId = PointId(i)
    val T = 200
    val cells = IndexedSeq.fill(T)(
      TetrahedralCell(rng.scalaRandom.nextInt(N),
                      rng.scalaRandom.nextInt(N),
                      rng.scalaRandom.nextInt(N),
                      rng.scalaRandom.nextInt(N)
      )
    )
    val list = TetrahedralList(cells)

    TetrahedralMesh3D(domain, list)
  }

  private def createRandomScalarVolumeMeshField(): ScalarVolumeMeshField[Int] = {
    // points around unit cube

    val tetraMesh = createRandomTetrahedralMesh()
    val scalars = tetraMesh.pointSet.points.map { p =>
      1
    }.toIndexedSeq
    DiscreteField(tetraMesh, scalars)
  }

}
