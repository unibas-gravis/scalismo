package scalismo.io.stl

import scalismo.geometry.Point3D
import scalismo.io.FileReader
import scalismo.mesh.TriangleMesh3D

import java.nio.ByteBuffer
import scala.util.Try

object STLMeshReaderBinary {
  def read(file: String): Try[TriangleMesh3D] = Try {
    val dataBuffer = FileReader.readFileToByteBuffer(file)
    dataBuffer.position(HEADER_LENGTH).order(ORDER)

    val numTriangles = readInt(dataBuffer)
    val trianglesArray = new Array[STLTriangle](numTriangles)

    val triangles = (0 until numTriangles).map { _ =>
      val n = readVertex(dataBuffer).toVector
      val p1 = readVertex(dataBuffer)
      val p2 = readVertex(dataBuffer)
      val p3 = readVertex(dataBuffer)
      readShort(dataBuffer)
      STLTriangle(n, p1, p2, p3)
    }
    STLHelpers.STLTrianglesToTriangleMesh(triangles)
  }

  private def readShort(bb: ByteBuffer): Short = {
    bb.getShort
  }

  private def readInt(bb: ByteBuffer): Int = {
    bb.getInt
  }

  private def readVertex(bb: ByteBuffer): Point3D = {
    Point3D(readFloat(bb), readFloat(bb), readFloat(bb))
  }

  private def readFloat(bb: ByteBuffer): Float = {
    bb.getFloat
  }
}
