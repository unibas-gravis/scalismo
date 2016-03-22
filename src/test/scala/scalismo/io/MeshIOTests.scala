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
import scalismo.common.ScalarArray
import scalismo.mesh.ScalarMeshField

import scala.language.implicitConversions

class MeshIOTests extends ScalismoTestSuite {

  describe("MeshIO") {

    it("yields the original mesh when reading  and writing") {
      val path = getClass.getResource("/facemesh.stl").getPath
      val origMesh = MeshIO.readMesh(new File(path)).get

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
  }

  describe("ScalarMeshField IO") {

    it("can write and correctly read a ScalarMeshField") {
      val path = getClass.getResource("/facemesh.stl").getPath
      val mesh = MeshIO.readMesh(new File(path)).get

      val meshData = ScalarMeshField(mesh, ScalarArray(mesh.pointIds.map(_.id).toArray))

      val tmpFile = File.createTempFile("scalarMesh", ".vtk")

      val writeTry = MeshIO.writeScalarMeshField(meshData, tmpFile)
      assert(writeTry.isSuccess)

      val readTry = MeshIO.readScalarMeshFieldAsType[Int](tmpFile)
      assert(readTry.isSuccess)

      readTry.get.data == ScalarArray(mesh.pointIds.map(_.id).toArray)
    }
  }

}