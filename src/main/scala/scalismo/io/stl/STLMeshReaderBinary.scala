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
package scalismo.io.stl

import scalismo.geometry.Point3D
import scalismo.io.FileReader
import scalismo.io.stl.STL.{STL_BYTE_ORDER, STL_HEADER_LENGTH}
import scalismo.io.stl.STLTriangle
import scalismo.mesh.TriangleMesh3D

import java.io.File
import java.nio.ByteBuffer
import scala.util.Try

object STLMeshReaderBinary {
  def read(file: File): Try[TriangleMesh3D] = Try {
    val dataBuffer = FileReader.readFileToByteBuffer(file.toString)
    dataBuffer.position(STL_HEADER_LENGTH).order(STL_BYTE_ORDER)

    val numTriangles = readInt(dataBuffer)
    val trianglesArray = new Array[STLTriangle](numTriangles)

    val triangles = (0 until numTriangles).map { _ =>
      val n = readVertex(dataBuffer).toVector
      val p1 = readVertex(dataBuffer)
      val p2 = readVertex(dataBuffer)
      val p3 = readVertex(dataBuffer)
      readShort(dataBuffer)
      STLTriangle(n, p1, p2, p3)
    }
    STLTriangle.STLTrianglesToTriangleMesh(triangles)
  }

  private def readShort(bb: ByteBuffer): Short = {
    bb.getShort
  }

  private def readInt(bb: ByteBuffer): Int = {
    bb.getInt
  }

  private def readVertex(bb: ByteBuffer): Point3D = {
    Point3D(readFloat(bb), readFloat(bb), readFloat(bb))
  }

  private def readFloat(bb: ByteBuffer): Float = {
    bb.getFloat
  }
}
