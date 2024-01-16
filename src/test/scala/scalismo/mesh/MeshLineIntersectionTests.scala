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
//package scalismo.mesh
//
//import java.io.File
//import java.net.URLDecoder
//
//import scalismo.ScalismoTestSuite
//import scalismo.geometry.EuclideanVector3D
//import scalismo.io.MeshIO
//import scalismo.utils.Random
//
//import scala.language.implicitConversions
//
//class MeshLineIntersectionTests extends ScalismoTestSuite {
//
//  implicit val rng: Random = Random(1024L)
//
//  describe("A intersection of a line with a tetrahedral mesh") {
//
//    object Fixture {
//
//      val path = getClass.getResource("/tetraMesh.vtk").getPath
//      val testMesh = MeshIO.readTetrahedralMesh(new File(URLDecoder.decode(path, "UTF-8"))).get
//
//    }
//
//    it("should contain a known intersection point") {
//      val mesh = Fixture.testMesh
//      for (_ <- 0 until 100) {
//        val tetId = TetrahedronId(rng.scalaRandom.nextInt(mesh.tetrahedralization.tetrahedrons.size))
//
//        // an intersection point will lie within the triangle
//        val tet = mesh.tetrahedralization.tetrahedron(tetId)
//        val tri = tet.triangles(rng.scalaRandom.nextInt(4))
//        val bc3 = BarycentricCoordinates.randomUniform
//        val v = mesh.pointSet.point(tri.ptId1).toVector * bc3.a +
//          mesh.pointSet.point(tri.ptId2).toVector * bc3.b +
//          mesh.pointSet.point(tri.ptId3).toVector * bc3.c
//        val intersectionPoint = v.toPoint
//
//        // random direction
//        val direction = EuclideanVector3D(
//          rng.scalaRandom.nextGaussian(),
//          rng.scalaRandom.nextGaussian(),
//          rng.scalaRandom.nextGaussian()
//        ).normalize
//
//        // select point on the line given by direction an intersection point
//        val anchorPoint = intersectionPoint + direction * rng.scalaRandom.nextDouble() * 100
//
//        // try to find the intersection point
//        val intersections = mesh.operations.getIntersectionPoints(anchorPoint, direction)
//        val distances = intersections.map(ip => (ip - intersectionPoint).norm)
//        val closestDistanceToTrueIntersectionPoint = distances.min
//        closestDistanceToTrueIntersectionPoint should be < 1.0e-8
//      }
//    }
//  }
//
//}
