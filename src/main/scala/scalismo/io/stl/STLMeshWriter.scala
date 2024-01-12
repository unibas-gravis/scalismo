package scalismo.io.stl

import scalismo.geometry.EuclideanVector3D
import scalismo.mesh.TriangleMesh3D

import java.io.{BufferedOutputStream, DataOutputStream, FileOutputStream, OutputStream}
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
  private def writeString(dos: DataOutputStream, data: String): Unit ={
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
  private def writeFloat(dos: DataOutputStream, data: Float): Unit ={
    dos.write(ByteBuffer.allocate(4).order(ORDER)
      .putFloat(data).array())
  }
  private def writeVertex(dos: DataOutputStream, vertex: EuclideanVector3D): Unit = {
    writeFloat(dos, vertex.x.toFloat)
    writeFloat(dos, vertex.y.toFloat)
    writeFloat(dos, vertex.z.toFloat)
  }
}
