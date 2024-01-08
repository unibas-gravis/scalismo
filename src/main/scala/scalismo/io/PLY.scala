package scalismo.io
import java.nio.charset.StandardCharsets
import scalismo.geometry._3D
import scalismo.mesh.{TriangleMesh, VertexColorMesh3D}

import java.io.{BufferedOutputStream, DataOutputStream, File, FileOutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scala.util.{Success, Try}

object PLY {

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    val faceMesh = MeshIO.readMesh(new File("src/test/resources/facemesh.stl")).get
    val outTest = new File("src/test/resources/facemesh_test.ply")
    val outTest2 = new File("src/test/resources/facemesh_test2.ply")
    val startTime = System.nanoTime()
    MeshIO.writeMesh(faceMesh, outTest2)
    val midTime = System.nanoTime()
    save(Left(faceMesh), outTest)
    val endTime = System.nanoTime()
    val elapsedTimeSeconds0 = (midTime - startTime) / 1e9
    val elapsedTimeSeconds1 = (endTime - midTime) / 1e9
    println(s"Elapsed Time: $elapsedTimeSeconds0 seconds")
    println(s"Elapsed Time: $elapsedTimeSeconds1 seconds")
  }

  def save(surface: Either[TriangleMesh[_3D], VertexColorMesh3D], file: File): Try[Unit] = {
    val mesh = surface match {
      case Right(colorMesh) => colorMesh.shape
      case Left(shapeOnly)  => shapeOnly
    }

    val headerContent = createHeader(mesh.pointSet.numberOfPoints, mesh.triangulation.triangles.length, false)

    Try {
      val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
      try {
        dos.write(headerContent.getBytes("UTF-8"))
        mesh.pointSet.points.foreach { p =>
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(p.x.toFloat).array())
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(p.y.toFloat).array())
          dos.write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(p.z.toFloat).array())
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

//  def load(file: File): Either[TriangleMesh[_3D], VertexColorMesh3D] = {}

  private def createHeader(numVertices: Int, numFaces: Int, vertexColors: Boolean): String = {
    val header = new StringBuilder
    header.append("ply\nformat binary_little_endian 1.0\ncomment Scalismo generated PLY File\n")
    header.append(f"element vertex $numVertices\nproperty float x\nproperty float y\nproperty float z\n")
    if (vertexColors) {
      header.append("property uchar r\nproperty uchar g\nproperty uchar b\nproperty uchar a\n")
    }
    header.append(f"element face $numFaces\nproperty list uchar int vertex_indices\nend_header\n")
    header.toString()
  }
}
