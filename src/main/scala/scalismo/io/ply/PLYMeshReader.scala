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
package scalismo.io.ply

import scalismo.color.RGBA
import scalismo.geometry._3D
import scalismo.mesh.{SurfacePointProperty, TriangleList, TriangleMesh, TriangleMesh3D, VertexColorMesh3D}

import java.io.{BufferedReader, File, FileReader, RandomAccessFile}
import java.nio.{ByteBuffer, ByteOrder}
import scala.util.{Failure, Success, Try}

object PLYMeshReader {

  def readFileAndParseHeader(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = Try {
    val breader = new BufferedReader(new FileReader(file))
    val lineIterator = Iterator.continually(breader.readLine())
    val headerLines = lineIterator.dropWhile(_ != "ply").takeWhile(_ != "end_header").toArray :+ "end_header"
    val headerInfo: HeaderInfo = PLYHeader.parseHeader(headerLines)

    val (points, colors, triangles) = if (headerInfo.format.format == PLYTypeFormat.ASCII) {
      val (points, colors) = PLYMeshReaderAscii.readPoints(lineIterator, headerInfo.vertexInfo)
      val triangles = PLYMeshReaderAscii.readTriangles(lineIterator, headerInfo.faceInfo)
      (points, colors, triangles)
    } else {
      val byteOrder =
        if (headerInfo.format.format == PLYTypeFormat.BINARY_LITTLE_ENDIAN) ByteOrder.LITTLE_ENDIAN
        else ByteOrder.BIG_ENDIAN

      val dataBuffer = readFileToByteBuffer(file.toString)
      dataBuffer.position(headerInfo.headerLength)
      dataBuffer.slice().order(byteOrder)

      val (points, colors) = PLYMeshReaderBinary.readPoints(dataBuffer, headerInfo.vertexInfo, byteOrder)
      val triangles = PLYMeshReaderBinary.readTriangles(dataBuffer, headerInfo.faceInfo, byteOrder)
      (points, colors, triangles)
    }

    val mesh = TriangleMesh3D(points, TriangleList(triangles))
    colors match {
      case Some(c) => Right(VertexColorMesh3D(mesh, SurfacePointProperty[RGBA](mesh.triangulation, c)))
      case None    => Left(mesh)
    }
  }

  private def readFileToByteBuffer(file: String): ByteBuffer = {
    val raf = new RandomAccessFile(file, "r")
    val channel = raf.getChannel
    val buffer = ByteBuffer.allocate(channel.size.toInt)
    channel.read(buffer)
    buffer.rewind()
    buffer
  }
}
