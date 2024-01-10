package scalismo.io.stl

import scalismo.geometry.Point3D
import scalismo.mesh.TriangleMesh3D

import java.nio.ByteBuffer

object STLMeshWriter {

  def write(name: String, mesh: TriangleMesh3D): Unit = {
//    try {
//      val capacity = math.max(80 + 4, 4 * 3 * 4 + 2)
//      val buffer = ByteBuffer.allocate(capacity)
//      buffer.order(ByteOrder.LITTLE_ENDIAN)
//
//      // Write the header.
//      val nameBytes = name.getBytes
//      val nameArray = Array.tabulate[Byte](80) { i =>
//        if (i < nameBytes.length) nameBytes(i) else 0
//      }
//      buffer.put(nameArray)
//      buffer.putInt(facets.size)
//      os.write(buffer.array, 0, buffer.position)
//
//      // Write facets.
//      facets.foreach { facet =>
//        buffer.rewind()
//        writeVertex(buffer, facet.normal)
//        writeVertex(buffer, facet.v1)
//        writeVertex(buffer, facet.v2)
//        writeVertex(buffer, facet.v3)
//        buffer.putShort(0)
//        os.write(buffer.array, 0, buffer.position)
//      }
//    } finally {
//      os.close()
//    }
  }

  private def writeVertex(buffer: ByteBuffer, vertex: Point3D): Unit = {
    buffer.putFloat(vertex.x.toFloat)
    buffer.putFloat(vertex.y.toFloat)
    buffer.putFloat(vertex.z.toFloat)
  }

}
