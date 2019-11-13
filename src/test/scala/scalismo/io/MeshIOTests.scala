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
import scalismo.common.{ Scalar, ScalarArray }
import scalismo.geometry._3D
import scalismo.mesh.{ ScalarMeshField, TriangleMesh }

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{ Failure, Success, Try }

class MeshIOTests extends ScalismoTestSuite {

  describe("MeshIO") {

    it("yields the original mesh when reading  and writing") {
      val path = getClass.getResource("/facemesh.stl").getPath
      val origMesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get

      def testWriteRead(extension: String): Unit = {
        val tmpFile = File.createTempFile("mesh", ".vtk")
        val writeStatus = MeshIO.writeVTK(origMesh, tmpFile)
        writeStatus.isSuccess should be(true)

        val meshTry = MeshIO.readMesh(tmpFile)
        meshTry.isSuccess should be(true)
        meshTry.map { mesh =>
          mesh should equal(origMesh)
        }
      }
      testWriteRead(".vtk")
      testWriteRead(".stl")

    }

    it("returns a Failure object instead of throwing an exception for nonexistent files") {
      val path = "/NONEXISTENT-b1cfa24000992425413ff27a52c07705ba507062a71efd7f924178972545bf7353d6ed78aea47a1.h5"
      val failed = MeshIO.readHDF5(new File(path))
      failed.isFailure should be(true)
    }

    it("yields the original polyline when reading  and writing a polyLine in 2D") {
      val path = getClass.getResource("/linemesh.vtk").getPath
      val origMesh = MeshIO.readLineMesh2D(new File(URLDecoder.decode(path, "UTF-8"))).get

      val tmpFile = File.createTempFile("mesh", ".vtk")
      val writeStatus = MeshIO.writeLineMesh(origMesh, tmpFile)
      writeStatus.isSuccess should be(true)

      val meshTry = MeshIO.readLineMesh2D(tmpFile)
      meshTry.isSuccess should be(true)
      meshTry.map { mesh =>
        mesh should equal(origMesh)
      }

    }

    it("yields the original mesh when reading and writing a shape only ply") {
      val path = getClass.getResource("/mean_shapeOnly.ply").getPath
      val shape = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
      val tmpFile = File.createTempFile("mesh", ".ply")
      MeshIO.writeMesh(shape, tmpFile)
      val reRead = MeshIO.readMesh(tmpFile).get
      tmpFile.delete()

      reRead should equal(shape)
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

    it("correctly fails when reading a textured ascii ply") {
      val path = getClass.getResource("/mean_textured.ply").getPath
      val shape = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8")))
      assert(shape.isFailure)
    }

  }

  describe("ScalarMeshField IO") {

    object Fixture {
      val path: String = getClass.getResource("/facemesh.stl").getPath
      val mesh: TriangleMesh[_3D] = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
      val meshData: ScalarMeshField[Int] = ScalarMeshField(mesh, ScalarArray(mesh.pointSet.pointIds.map(_.id).toArray))
    }

    def sameWriteRead[S: Scalar: TypeTag: ClassTag](): Try[ScalarMeshField[S]] = {
      val f = Fixture
      val tmpFile = File.createTempFile("scalarMesh", ".vtk")

      val writeTry = MeshIO.writeScalarMeshField(f.meshData, tmpFile)
      assert(writeTry.isSuccess)

      MeshIO.readScalarMeshField[S](tmpFile)
    }

    it("can write and correctly read a ScalarMeshField") {
      val f = Fixture
      val readTry = sameWriteRead[Int]()
      assert(readTry.isSuccess)

      readTry.get.data == ScalarArray(f.mesh.pointSet.pointIds.map(_.id).toArray)
    }

    it("fails when reading with the wrong Scalar type") {
      val readTry = sameWriteRead[Float]()
      assert(readTry.isFailure)
    }

    it("can read and cast a ScalarMeshField") {
      val f = Fixture
      val tmpFile = File.createTempFile("scalarMesh", ".vtk")
      tmpFile.deleteOnExit()
      MeshIO.writeScalarMeshField(f.meshData, tmpFile)

      MeshIO.readScalarMeshFieldAsType[Float](tmpFile) match {
        case Success(meshData) =>
          for ((origDatum, newDatum) <- f.meshData.data.zip(meshData.data)) {
            origDatum.toFloat should equal(newDatum)
          }
        case Failure(t) =>
          println(t.getMessage)
          t.printStackTrace()
          throw t
      }

    }

  }

}