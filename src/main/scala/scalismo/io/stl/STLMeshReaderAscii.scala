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

import scalismo.geometry.{EuclideanVector, EuclideanVector3D, Point3D}
import scalismo.mesh.TriangleMesh3D

import java.io.{BufferedReader, FileReader, IOException}
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object STLMeshReaderAscii {
  def read(file: String): Try[TriangleMesh3D] = Try {
    val breader = new BufferedReader(new FileReader(file))
    val header = breader.readLine()

    val triangles = ArrayBuffer.empty[STLTriangle]
    var line: String = null
    def hasLineToProcess(): Boolean = {
      line = breader.readLine();
      line != null && !line.trim.startsWith("endsolid")
    }
    while (hasLineToProcess()) {
      line = line.trim.replaceAll(" +", " ")
      val triangleStrings: Array[String] = Array(line) ++
        (0 until 6).map(_ => breader.readLine().trim.replaceAll(" +", " ")).toArray
      val triangle = parseTriangleStrings(triangleStrings)
      triangles += triangle
    }
    breader.close()

    STLTriangle.STLTrianglesToTriangleMesh(triangles.toSeq)
  }

  private def parseTriangleStrings(data: Seq[String]): STLTriangle = {
    if (data.length != 7) {
      throw new IOException("Wrong faces description format does not include all 7 descriptors.")
    }

    // NOTE 2024-01-21 (Andreas Morel-Forster): Maybe not needed checks. Trade-off format checking vs. performance.
    checkTag(data(1), "outer loop")
    checkTag(data(5), "endloop")
    checkTag(data(6), "endfacet")

    STLTriangle(
      parseNormalString(data(0)),
      parseVertexString(data(2)),
      parseVertexString(data(3)),
      parseVertexString(data(4))
    )
  }

  private def checkTag(part: String, tag: String) = {
    if (!part.startsWith(tag)) {
      throw new IOException(f"Wrong start of line, expected ${tag} but start is ${part.take(tag.length)}")
    }
  }

  private def splitChecked(part: String, tag: String, nElements: Int) = {
    if (!part.startsWith(tag)) {
      throw new IOException(f"Wrong identifier at beginning, expected ${tag} but start is ${part.take(tag.length)}")
    }

    val parts = part.replace(tag, "").trim.split(" ")
    if (parts.length != nElements) {
      throw new IOException(f"Wrong number of elements for ${tag}. Found ${parts.length}, expected ${nElements}")
    }
    parts
  }

  private def parseNormalString(part: String): EuclideanVector3D = {
    val tag = "facet normal"
    val expectedValues = 3
    val parts = splitChecked(part, tag, expectedValues).map(_.toDouble)
    EuclideanVector3D(parts(0), parts(1), parts(2))
  }

  private def parseVertexString(part: String): Point3D = {
    val tag = "vertex"
    val expectedValues = 3
    val parts = splitChecked(part, tag, expectedValues).map(_.toDouble)
    Point3D(parts(0), parts(1), parts(2))
  }
}
