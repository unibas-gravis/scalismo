package smptk.utils

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smptk.geometry._
import smptk.geometry.implicits._
import vtk.vtkPolyDataReader
import smptk._
import smptk.io.MeshIO

class ConversionTests extends FunSpec with ShouldMatchers {
  smptk.initialize()

  describe("a Mesh ") {

    it("can be converted to and from vtk") {
      //val path = getClass().getResource("/facemesh.h5").getPath
      val origmesh = MeshIO.readHDF5(new java.io.File("/tmp/test.h5")).get
      val vtkpd = MeshConversion.meshToVTKPolyData(origmesh)            
      val restoredMesh = MeshConversion.vtkPolyDataToTriangleMesh(vtkpd).get
      origmesh should equal(restoredMesh)
    }
  }
}