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
import org.statismo.stk.core.geometry.implicits._
import org.statismo.stk.core.mesh.ScalarMeshData
import org.statismo.stk.core.common.ScalarValue._

class MeshOTests extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize
  
  describe("MeshIO") {
    // TODO add a test to test reading and writing
    it("can write a scalar function pseudo test (TODO do me right)") { 
          val path = getClass().getResource("/facemesh.h5").getPath
      val facemesh = MeshIO.readHDF5(new File(path)).get
      val scalars = (0 until facemesh.numberOfPoints).map(id => id).toArray
      val md = ScalarMeshData(facemesh, scalars)
      println(MeshIO.writeMeshData(md, new File("d:\\temp\\x.vtk")))
    }
  }

 

}