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

import scalismo.geometry.EuclideanVector3D
import scalismo.mesh.TriangleMesh3D

import java.io.{BufferedOutputStream, DataOutputStream, FileOutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Try

object STLMeshWriter {
  private val ORDER = ByteOrder.LITTLE_ENDIAN

  def write(mesh: TriangleMesh3D, file: String, header: String): Try[Unit] = Try {
    val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    val headerMaxLength = 80
    val headerCapped = header.take(headerMaxLength).padTo(headerMaxLength, ' ')
    writeString(dos, headerCapped)
    writeInt(dos, mesh.triangulation.triangles.length)
    mesh.triangulation.triangleIds.foreach { id =>
      val facet = mesh.triangulation.triangle(id)
      val n = mesh.cellNormals.onTriangle(id)
      val p1 = mesh.pointSet.point(facet.ptId1).toVector
      val p2 = mesh.pointSet.point(facet.ptId2).toVector
      val p3 = mesh.pointSet.point(facet.ptId3).toVector
      writeVertex(dos, n)
      writeVertex(dos, p1)
      writeVertex(dos, p2)
      writeVertex(dos, p3)
      writeShort(dos, 0)
    }
    dos.close()
  }

  private def writeString(dos: DataOutputStream, data: String): Unit = {
    dos.write(ByteBuffer.allocate(data.getBytes.length)
      .order(ORDER).put(data.getBytes("UTF-8")).array())
  }

  private def writeShort(dos: DataOutputStream, data: Short): Unit = {
    dos.write(ByteBuffer.allocate(2).order(ORDER)
      .putShort(data).array())
  }

  private def writeInt(dos: DataOutputStream, data: Int): Unit = {
    dos.write(ByteBuffer.allocate(4).order(ORDER)
      .putInt(data).array())
  }

  private def writeVertex(dos: DataOutputStream, vertex: EuclideanVector3D): Unit = {
    writeFloat(dos, vertex.x.toFloat)
    writeFloat(dos, vertex.y.toFloat)
    writeFloat(dos, vertex.z.toFloat)
  }

  private def writeFloat(dos: DataOutputStream, data: Float): Unit = {
    dos.write(ByteBuffer.allocate(4).order(ORDER)
      .putFloat(data).array())
  }
}
