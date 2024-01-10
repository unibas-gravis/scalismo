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

// The STL implementation is based on the ScalaCad implementation: https://github.com/joewing/ScalaCad
package scalismo.io.stl

import scalismo.mesh.{TriangleList, TriangleMesh3D}

object STLMesh {
  def writeSTL(mesh: TriangleMesh3D, filename: String): Unit = {
//    StlBinaryFileWriter.write(r, os)
  }

  def readTriangleMesh3D(filename: String): TriangleMesh3D = {
    TriangleMesh3D(IndexedSeq(), TriangleList(IndexedSeq()))
  }
}
