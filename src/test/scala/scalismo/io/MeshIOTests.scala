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

import scalismo.ScalismoTestSuite
import scalismo.common.*
import scalismo.common.DiscreteField.{ScalarMeshField, ScalarVolumeMeshField}
import scalismo.geometry.{_3D, Point}
import scalismo.mesh.*
import scalismo.utils.Random

import java.io.File
import java.net.URLDecoder
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class MeshIOTests extends ScalismoTestSuite {

  describe("MeshIO") {

    it("yields the original mesh when reading and writing") {
      val path = getClass.getResource("/facemesh.stl").getPath
      val origMesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get

      def testWriteRead(extension: String): Unit = {
        val tmpFile = File.createTempFile("mesh", extension)
        val writeStatus = MeshIO.writeMesh(origMesh, tmpFile)
        writeStatus.isSuccess should be(true)

        val meshTry = MeshIO.readMesh(tmpFile)
        meshTry.isSuccess should be(true)
        meshTry.map { mesh =>
          mesh should equal(origMesh)
        }
      }

      testWriteRead(".stl")
      testWriteRead(".ply")
    }

    it("stl and ply readers (binary and ascii) yield the same result") {
      val pathSTL = getClass.getResource("/unit-sphere.stl").getPath
      val pathSTLascii = getClass.getResource("/unit-sphere_ascii.stl").getPath
      val pathPLY = getClass.getResource("/unit-sphere.ply").getPath
      val pathPLYascii = getClass.getResource("/unit-sphere_ascii.ply").getPath
      val meshSTL = MeshIO.readMesh(new File(URLDecoder.decode(pathSTL, "UTF-8"))).get
      val meshSTLascii = MeshIO.readMesh(new File(URLDecoder.decode(pathSTLascii, "UTF-8"))).get
      val meshPLY = MeshIO.readMesh(new File(URLDecoder.decode(pathPLY, "UTF-8"))).get
      val meshPLYascii = MeshIO.readMesh(new File(URLDecoder.decode(pathPLYascii, "UTF-8"))).get

      def compareMeshes(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]): Unit = {
        m1.triangulation.triangles.toSet should equal(m2.triangulation.triangles.toSet)
        m1.pointSet.points.toSeq.zip(m2.pointSet.points.toSeq).foreach { case (p1, p2) =>
          p1.x should be(p2.x +- 1e-6)
          p1.y should be(p2.y +- 1e-6)
          p1.z should be(p2.z +- 1e-6)
        }
      }

      compareMeshes(meshSTL, meshSTLascii)
      compareMeshes(meshSTL, meshPLY)
      compareMeshes(meshSTL, meshPLYascii)
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

    it("does not throw an exception when called with an nonexistant file") {
      noException should be thrownBy MeshIO.readMesh(new java.io.File("idonotexist.ply"))
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

    it("correctly reads textured mesh and discards unsupported properties") {
      val pathTextured = getClass.getResource("/mean_textured.ply").getPath
      val pathVertexColor = getClass.getResource("/mean_vertexColor.ply").getPath
      val shapeTexture = MeshIO.readMesh(new File(URLDecoder.decode(pathTextured, "UTF-8"))).get
      val shapeVertexColor = MeshIO.readMesh(new File(URLDecoder.decode(pathVertexColor, "UTF-8"))).get
      shapeTexture should equal(shapeVertexColor)
    }
  }
}

object MeshIOTests {
  def createRandomTetrahedralMesh(): TetrahedralMesh3D = {
    // points around unit cube

    val rng = Random(42L)
    val N = 200
    val points = IndexedSeq.fill(N)(
      Point(rng.scalaRandom.nextGaussian() * 2,
            rng.scalaRandom.nextGaussian() * 100,
            rng.scalaRandom.nextGaussian() * 1000
      )
    )
    val domain = UnstructuredPoints(points)

    // cells covering the complete cube
    implicit def intToPointId(i: Int): PointId = PointId(i)
    val T = 200
    val cells = IndexedSeq.fill(T)(
      TetrahedralCell(rng.scalaRandom.nextInt(N),
                      rng.scalaRandom.nextInt(N),
                      rng.scalaRandom.nextInt(N),
                      rng.scalaRandom.nextInt(N)
      )
    )
    val list = TetrahedralList(cells)

    TetrahedralMesh3D(domain, list)
  }

  private def createRandomScalarVolumeMeshField(): ScalarVolumeMeshField[Int] = {
    // points around unit cube

    val tetraMesh = createRandomTetrahedralMesh()
    val scalars = tetraMesh.pointSet.points.map { p =>
      1
    }.toIndexedSeq
    DiscreteField(tetraMesh, scalars)
  }

}
