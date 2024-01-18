package scalismo.io.ply

import scalismo.color.RGBA
import scalismo.geometry._3D
import scalismo.mesh.{TriangleMesh, TriangleMesh3D, VertexColorMesh3D}

import java.io.{BufferedOutputStream, DataOutputStream, File, FileOutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import scala.util.Try

object PLYMeshWriter {

  def write(mesh: TriangleMesh[_3D], colors: Option[Iterator[RGBA]], file: File): Try[Unit] = {
    val hasColor = colors.isDefined
    Try {
      val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))

      val headerContent =
        PLYHeader.createHeader(mesh.pointSet.numberOfPoints, mesh.triangulation.triangles.length, hasColor)

      try {
        val colorIterator = colors.getOrElse(Iterator())

        dos.write(headerContent.getBytes("UTF-8"))
        mesh.pointSet.points.foreach { p =>
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(p.x.toFloat).array())
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(p.y.toFloat).array())
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(p.z.toFloat).array())

          if (hasColor) {
            val c = colorIterator.next()
            dos.writeByte((c.r * 255).toByte)
            dos.writeByte((c.g * 255).toByte)
            dos.writeByte((c.b * 255).toByte)
            dos.writeByte((c.a * 255).toByte)
          }
        }
        mesh.triangulation.triangles.foreach { t =>
          dos.writeByte(3)
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(t.ptId1.id).array())
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(t.ptId2.id).array())
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(t.ptId3.id).array())
        }
      } finally {
        dos.close()
      }
    }
  }
}
