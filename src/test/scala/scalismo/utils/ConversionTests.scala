
package scalismo.utils

import scalismo.geometry._2D
import scalismo.io.{ImageIO, MeshIO}

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ConversionTests extends FunSpec with ShouldMatchers {
  scalismo.initialize()

  describe("a Mesh ") {

    it("can be converted to and from vtk") {
      val path = getClass().getResource("/facemesh.h5").getPath
      val origmesh = MeshIO.readHDF5(new java.io.File(path)).get
      val vtkpd = MeshConversion.meshToVTKPolyData(origmesh)
      val restoredMesh = MeshConversion.vtkPolyDataToTriangleMesh(vtkpd).get
      origmesh should equal(restoredMesh)

      // test conversion with template
      val vtkpd2 = MeshConversion.meshToVTKPolyData(origmesh, Some(vtkpd))
      val restoredMesh2 = MeshConversion.vtkPolyDataToTriangleMesh(vtkpd2).get
      origmesh should equal(restoredMesh2)

    }
  }
  describe("an 2D image") {
    it("can be converted to and from vtk") {
      val path = getClass().getResource("/lena.h5").getPath
      val origimg = ImageIO.read2DScalarImage[Short](new java.io.File(path)).get
      val vtksp = ImageConversion.imageTovtkStructuredPoints(origimg)
      val restoredImg = ImageConversion.vtkStructuredPointsToScalarImage[_2D, Short](vtksp).get

      origimg should equal(restoredImg)

    }
  }

}