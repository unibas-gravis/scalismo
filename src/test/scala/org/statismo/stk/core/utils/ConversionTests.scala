
package org.statismo.stk.core.utils

import scala.language.implicitConversions
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.implicits._
import vtk.vtkPolyDataReader
import org.statismo.stk.core._
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.io.ImageIO

class ConversionTests extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

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
      val vtksp = ImageConversion.image2DTovtkStructuredPoints(origimg)
      val restoredImg = ImageConversion.vtkStructuredPointsTo2DScalarImage[Short](vtksp).get

      println("***** restored domain parameters " + restoredImg.domain.anisotropSimTransformParameters)
      println("***** original  domain parameters " + origimg.domain.anisotropSimTransformParameters)
      origimg should equal(restoredImg)

    }
  }

}