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

import scalismo.geometry._3D
import scalismo.mesh.{TriangleMesh, VertexColorMesh3D}

import java.io.{File, IOException}
import scala.util.{Failure, Try}

object PLYMesh {

  def write(mesh: TriangleMesh[_3D], filename: File): Try[Unit] = {
    PLYMeshWriter.write(mesh, None, filename)
  }

  def write(mesh: VertexColorMesh3D, filename: File): Try[Unit] = {
    PLYMeshWriter.write(mesh.shape, Option(mesh.color.pointData.iterator), filename)
  }

  def read(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = {
    if (!file.exists()) {
      val filename = file.getCanonicalFile
      Failure(new IOException(s"Could not read ply file with name $filename. Reason: The file does not exist"))
    } else {
      PLYMeshReader.readFileAndParseHeader(file)
    }
  }
}
