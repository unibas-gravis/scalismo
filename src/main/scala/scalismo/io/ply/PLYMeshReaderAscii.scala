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
import scala.util.Try

object PLYMeshReaderAscii {

  def readPoints(lineIterator: Iterator[String],
                 vertexInfo: PLYElement
  ): (IndexedSeq[Point3D], Option[IndexedSeq[RGBA]]) = {
    val pIndex = PLYHelpers.getVertexPropertyIndex(vertexInfo.properties)
    val data: IndexedSeq[(Point3D, Option[RGBA])] = (0 until vertexInfo.count).map { _ =>
      val line = lineIterator.next()
      val split = line.split(" ")
      val p = Point3D(
        split(pIndex.x).toFloat,
        split(pIndex.y).toFloat,
        split(pIndex.z).toFloat
      )
      val c = if (pIndex.isRGB) {
        Option(
          RGBA(
            split(pIndex.red.get).toDouble / 255.0,
            split(pIndex.green.get).toDouble / 255.0,
            split(pIndex.blue.get).toDouble / 255.0,
            1.0
          )
        )
      } else if (pIndex.isRGBA) {
        Option(
          RGBA(
            split(pIndex.red.get).toDouble / 255.0,
            split(pIndex.green.get).toDouble / 255.0,
            split(pIndex.blue.get).toDouble / 255.0,
            split(pIndex.alpha.get).toDouble / 255.0
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

  def readTriangles(lineIterator: Iterator[String], faceInfo: PLYElement): IndexedSeq[TriangleCell] = {
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
        val line = lineIterator.next()
        val split = line.split(" ")
        if (split.isEmpty) {
          throw new IOException(
            "Faces property line is empty."
          )
        }
        var cnt = 0
        val vertexData = faceInfo.properties.map { property =>
          if (property.isList) {
            val listLength = split(cnt).toInt
            cnt += 1
            (0 until listLength).map { _ =>
              val item = split(cnt)
              cnt += 1
              item
            }
          } else {
            val item = split(cnt)
            cnt += 1
            Seq(item)
          }
        }
        TriangleCell(
          PointId(vertexData(vertexIndex)(0).toInt),
          PointId(vertexData(vertexIndex)(1).toInt),
          PointId(vertexData(vertexIndex)(2).toInt)
        )
      }
    }
  }
}
