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

import scalismo.mesh.TriangleMesh3D

import java.io.{BufferedReader, FileReader}
import scala.util.Try

object STLMesh {
  def write(mesh: TriangleMesh3D, filename: String): Try[Unit] = {
    STLMeshWriter.write(mesh, filename, "This is a header")
  }

  def read(filename: String): Try[TriangleMesh3D] = {
    val breader = new BufferedReader(new FileReader(filename))
    val fileType = breader.readLine().take(5)
    if (fileType == "solid") {
      STLMeshReaderAscii.read(filename)
    }
    else {
      STLMeshReaderBinary.read(filename)
    }
  }
}
