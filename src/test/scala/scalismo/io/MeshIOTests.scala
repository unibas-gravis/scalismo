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

import scala.language.implicitConversions
import org.scalatest.{ Matchers, FunSpec, FailureMessages }
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException

class MeshIOTests extends FunSpec with Matchers {

  scalismo.initialize()

  describe("MeshIO") {
    //    // TODO add a test to test reading and writing
    //    it("can write a scalar function pseudo test (TODO do me right)") {
    //          val path = getClass().getResource("/facemesh.h5").getPath
    //      val facemesh = MeshIO.readHDF5(new File(path)).get
    //      val scalars = (0 until facemesh.numberOfPoints).map(id => id).toArray
    //      val md = ScalarMeshData(facemesh, scalars)
    //      println(MeshIO.writeMeshData(md, new File("d:\\temp\\x.vtk")))
    //    }
    //

    it("yields the original mesh when readeing  and writing") {
      val path = getClass().getResource("/facemesh.stl").getPath
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

}