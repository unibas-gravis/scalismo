package org.statismo.stk.core.io

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.scalatest.FailureMessages
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.Point._
import org.statismo.stk.core.geometry.Vector._
import org.statismo.stk.core.geometry.Index._

import org.statismo.stk.core.mesh.ScalarMeshData
import org.statismo.stk.core.common.ScalarValue._

class MeshIOTests extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize
  
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
      val path = getClass().getResource("/facemesh.h5").getPath
      val origMesh = MeshIO.readHDF5(new File(path)).get

      def testWriteRead(extension : String) : Unit = {
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