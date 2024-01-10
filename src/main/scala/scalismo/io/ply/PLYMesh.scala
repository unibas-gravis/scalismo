/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package scalismo.io.ply

import scalismo.io.ply.PlyHelpers.PlyFormat._
import scalismo.io.ply.PlyHelpers.PlyHeader._
import scalismo.io.ply.PlyHelpers._
import scalismo.io.ply.PlyMeshReader._
import scalismo.mesh.{SurfacePointProperty, TriangleMesh3D, VertexColorMesh3D}

import scala.util.Try

/**
 * Function to write different type of meshes to a ply file. The supported formats are ASCII, BinaryBigEndian and
 * BinaryLittleEndian with headers that are understood by either meshlab or blender.
 */
object PLYMesh {

  def writePLY(mesh: TriangleMesh3D, fileURL: String): Unit =
    writePLY(mesh, fileURL, PlyFormat.BinaryLittleEndian, PlyHeader.meshlab)

  def writePLY(mesh: VertexColorMesh3D, fileURL: String): Unit =
    writePLY(mesh, fileURL, PlyFormat.BinaryLittleEndian, PlyHeader.meshlab)

  def writePLY(mesh: TriangleMesh3D, fileURL: String, plyFormat: PlyFormat, headerFormat: PlyHeader): Unit = {
    val points = mesh.pointSet.points.toIndexedSeq
    val triangles = mesh.triangulation.triangles.map(_.toIntVector3D)
    val writer = new PlyMeshWriter(
      url = fileURL,
      vertices = Option(points),
      faces = Option(triangles),
      plyFormat = plyFormat,
      headerFormat = headerFormat
    )
    writer.write(fileURL)
  }

  def writePLY(mesh: VertexColorMesh3D, fileURL: String, plyFormat: PlyFormat, headerFormat: PlyHeader): Unit = {
    val points = mesh.shape.pointSet.points.toIndexedSeq
    val triangles = mesh.shape.triangulation.triangles.map(_.toIntVector3D)
    val colors = mesh.color
    val writer = new PlyMeshWriter(
      url = fileURL,
      vertices = Option(points),
      faces = Option(triangles),
      color = Option(colors),
      plyFormat = plyFormat,
      headerFormat = headerFormat
    )
    writer.write(fileURL)
  }

  def readTriangleMesh3D(filename: String): Try[TriangleMesh3D] = Try {
    // read the data from the ply file
    val values = PlyReader.read(filename)

    // interpret the data
    val (vertexProperties, faceProperties) = getProperties(values)

    val points = getVertices(vertexProperties)
    val triangles = getTriangles(faceProperties)

    // build the mesh
    TriangleMesh3D(points, triangles)
  }

  def readVertexColorMesh3D(filename: String): Try[VertexColorMesh3D] = Try {
    // read data from the ply file
    val values = PlyReader.read(filename)

    // interpret the data
    val (vertexProperties, faceProperties) = getProperties(values)

    val points = getVertices(vertexProperties)
    val triangles = getTriangles(faceProperties)
    val triangleMesh3D = TriangleMesh3D(points, triangles)

    val colors = getColors(vertexProperties)

    // build the mesh
    VertexColorMesh3D(triangleMesh3D, SurfacePointProperty(triangles, colors))
  }

}
