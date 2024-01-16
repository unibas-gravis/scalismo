///*
// * Copyright 2015 University of Basel, Graphics and Vision Research Group
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package scalismo.utils
//
//import java.net.URLDecoder
//
//import scalismo.ScalismoTestSuite
//import scalismo.geometry._2D
//import scalismo.io.{ImageIO, MeshIO}
//
//class ConversionTests extends ScalismoTestSuite {
//
//  describe("a Mesh ") {
//
//    it("can be converted to and from vtk") {
//      val path = getClass.getResource("/facemesh.stl").getPath
//      val origmesh = MeshIO.readMesh(new java.io.File(URLDecoder.decode(path, "UTF-8"))).get
//      val vtkpd = MeshConversion.meshToVtkPolyData(origmesh)
//      val restoredMesh = MeshConversion.vtkPolyDataToTriangleMesh(vtkpd).get
//      origmesh should equal(restoredMesh)
//
//      // test conversion with template
//      val vtkpd2 = MeshConversion.meshToVtkPolyData(origmesh, Some(vtkpd))
//      val restoredMesh2 = MeshConversion.vtkPolyDataToTriangleMesh(vtkpd2).get
//      origmesh should equal(restoredMesh2)
//
//    }
//  }
//  describe("an 2D image") {
//    it("can be converted to and from vtk") {
//      val path = getClass.getResource("/lena.vtk").getPath
//      val origimg = ImageIO.read2DScalarImage[Short](new java.io.File(URLDecoder.decode(path, "UTF-8"))).get
//      val vtksp = ImageConversion.imageToVtkStructuredPoints(origimg, ImageConversion.VtkCubicInterpolation)
//      val restoredImg = ImageConversion.vtkStructuredPointsToScalarImage[_2D, Short](vtksp).get
//
//      origimg should equal(restoredImg)
//
//    }
//  }
//
//  describe("a tetrahedral mesh ") {
//
//    it("can be converted to and from vtk") {
//      val path = getClass.getResource("/tetraMesh.vtk").getPath
//      val origmesh = MeshIO.readTetrahedralMesh(new java.io.File(URLDecoder.decode(path, "UTF-8"))).get
//
//      val vtkug = TetrahedralMeshConversion.tetrahedralMeshToVTKUnstructuredGrid(origmesh)
//      val restoredMesh = TetrahedralMeshConversion.vtkUnstructuredGridToTetrahedralMesh(vtkug).get
//      origmesh should equal(restoredMesh)
//
//      // test conversion with template
//      val vtkug2 = TetrahedralMeshConversion.tetrahedralMeshToVTKUnstructuredGrid(origmesh, Some(vtkug))
//      val restoredMesh2 = TetrahedralMeshConversion.vtkUnstructuredGridToTetrahedralMesh(vtkug2).get
//      origmesh should equal(restoredMesh2)
//
//    }
//  }
//
//}
