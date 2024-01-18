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
import scalismo.common.PointId
import scalismo.geometry.Point3D
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh3D}

import java.io.IOException
import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Try

object PLYMeshReaderBinary {
  def readPoints(buffer: ByteBuffer,
                 vertexInfo: PLYElement,
                 byteOrder: ByteOrder
  ): (IndexedSeq[Point3D], Option[IndexedSeq[RGBA]]) = {
    val pIndex = PLYHelpers.getVertexPropertyIndex(vertexInfo.properties)
    val data: IndexedSeq[(Point3D, Option[RGBA])] = (0 until vertexInfo.count).map { _ =>
      val data = vertexInfo.properties.map { item =>
        item.read(buffer, byteOrder)
      }
      val p = Point3D(
        data(pIndex.x).head.asInstanceOf[Float],
        data(pIndex.y).head.asInstanceOf[Float],
        data(pIndex.z).head.asInstanceOf[Float]
      )
      val c = if (pIndex.isRGB) {
        Option(
          RGBA(
            data(pIndex.red.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.green.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.blue.get).head.asInstanceOf[Int].toDouble / 255.0,
            1.0
          )
        )
      } else if (pIndex.isRGBA) {
        Option(
          RGBA(
            data(pIndex.red.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.green.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.blue.get).head.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.alpha.get).head.asInstanceOf[Int].toDouble / 255.0
          )
        )
      } else {
        None
      }
      (p, c)
    }
    val (points, colors) = data.unzip
    val flattenedColors: Option[IndexedSeq[RGBA]] = if (colors.exists(_.isEmpty)) None else Some(colors.flatten)
    (points, flattenedColors)
  }

  def readTriangles(buffer: ByteBuffer, faceInfo: PLYElement, byteOrder: ByteOrder): IndexedSeq[TriangleCell] = {
    if (faceInfo.count == 0) {
      IndexedSeq.empty
    } else {
      val vertexIndex = PLYHelpers
        .getFacePropertyIndex(faceInfo.properties)
        .vertexIndex
        .getOrElse(
          throw new IOException(
            "Faces property vertex_index not found."
          )
        )
      (0 until faceInfo.count).map { _ =>
        val data = faceInfo.properties.map { item =>
          item.read(buffer, byteOrder)
        }
        val vertexData = data(vertexIndex)
        if (vertexData.length != 3) {
          throw new IOException(
            "Faces elements different than 3 specified."
          )
        }
        TriangleCell(PointId(vertexData(0).asInstanceOf[Int]),
                     PointId(vertexData(1).asInstanceOf[Int]),
                     PointId(vertexData(2).asInstanceOf[Int])
        )
      }
    }
  }
}
