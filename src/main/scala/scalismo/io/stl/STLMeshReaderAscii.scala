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

import scalismo.geometry.{EuclideanVector3D, Point3D}
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
    while ( hasLineToProcess() ) {
      line = line.trim.replaceAll(" +", " ")
      val triangleStrings: Array[String] = Array(line) ++
        (0 until 6).map(_ => breader.readLine().trim.replaceAll(" +", " ")).toArray
      val triangle = parseTriangleStrings(triangleStrings)
      triangles += triangle
    }
    breader.close()

    STLHelpers.STLTrianglesToTriangleMesh(triangles.toSeq)
  }

  private def parseTriangleStrings(data: Seq[String]): STLTriangle = {
    if (data.length != 7){
      throw new IOException("Wrong faces description format does not include all 7 descriptors.")
    }
    val filtered3Ddata = data.zip(TRIANGLE_LINE_DESCRIPTIONS).map{ case (s, desc) =>
      if(!s.startsWith(desc)){
        throw new IOException(f"Wrong faces description format, does not include ${desc}.")
      }
      val split = s.replace(desc, "").split(" ").filter(f => f != " ")
      if(split.length != 3){
        throw new IOException(f"Wrong number of faces items for ${desc}.")
      }
      split
    }
    STLTriangle(
      parseNormalString(filtered3Ddata(0)),
      parseVertexString(filtered3Ddata(2)),
      parseVertexString(filtered3Ddata(3)),
      parseVertexString(filtered3Ddata(4))
    )
  }

  private def parseNormalString(parts: Seq[String]): EuclideanVector3D = {
      EuclideanVector3D(parts(0).toFloat, parts(1).toFloat, parts(2).toFloat)
  }

  private def parseVertexString(parts: Seq[String]): Point3D = {
      Point3D(parts(1).toFloat, parts(1).toFloat, parts(2).toFloat)
  }
}
