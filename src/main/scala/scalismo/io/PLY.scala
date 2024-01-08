package scalismo.io
import scalismo.common.PointId

import java.nio.charset.StandardCharsets
import scalismo.geometry.{Point3D, _3D}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D, VertexColorMesh3D}

import java.io.{BufferedOutputStream, BufferedReader, DataOutputStream, File, FileOutputStream, FileReader, IOException}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scala.io.Source
import scala.util.{Failure, Success, Try}

object PLY {

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    val faceFile = new File("src/test/resources/facemesh.stl")
    val outTest = new File("src/test/resources/facemesh_test.ply")
    val outTest2 = new File("src/test/resources/facemesh_test2.ply")
//    doSomeWriting(faceFile, outTest, outTest2)
    doSomeReading(outTest, outTest2)
  }

  def doSomeReading(input1: File, input2: File): Unit = {
    println("Reading stuff")
    val startTime = System.nanoTime()
    val m1 = MeshIO.readMesh(input1)
    val midTime = System.nanoTime()
    val m2 = load(input2)
    val endTime = System.nanoTime()
    val elapsedTimeSeconds0 = (midTime - startTime) / 1e9
    val elapsedTimeSeconds1 = (endTime - midTime) / 1e9
    println(s"Elapsed Time: $elapsedTimeSeconds0 seconds")
    println(s"Elapsed Time: $elapsedTimeSeconds1 seconds")
//    println(m1.get == m2.get)
  }

  def doSomeWriting(input: File, output1: File, output2: File): Unit ={
    val faceMesh = MeshIO.readMesh(input).get
    println("Writing stuff")
    val startTime = System.nanoTime()
    MeshIO.writeMesh(faceMesh, output1)
    val midTime = System.nanoTime()
    save(Left(faceMesh), output2)
    val endTime = System.nanoTime()
    val elapsedTimeSeconds0 = (midTime - startTime) / 1e9
    val elapsedTimeSeconds1 = (endTime - midTime) / 1e9
    println(s"Elapsed Time: $elapsedTimeSeconds0 seconds")
    println(s"Elapsed Time: $elapsedTimeSeconds1 seconds")
  }

  private def load(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = {

    // read the ply header to find out if the ply is a textured mesh in ASCII (in which case we return a failure since VTKPLYReader Update() would crash otherwise)
    // vtk loader: https://kitware.github.io/vtk-js/api/IO_Geometry_PLYReader.html 
    if (!file.exists()) {
      val filename = file.getCanonicalFile
      Failure(new IOException(s"Could not read ply file with name $filename. Reason: The file does not exist"))
    } else {
      val breader = new BufferedReader(new FileReader(file))
      val lineIterator = Iterator.continually(breader.readLine())

      val headerLines = lineIterator.dropWhile(_ != "ply").takeWhile(_ != "end_header").toIndexedSeq

      if (headerLines.exists(_.contains("TextureFile")) && headerLines.exists(_.contains("format ascii"))) {
        Failure(
          new IOException(
            "PLY file $filename seems to be a textured mesh in ASCII format which creates issues with the VTK ply reader. Please convert it to a binary ply or to a vertex color or shape only ply."
          )
        )
      } else {
        println(headerLines)
        Try{
          Left(TriangleMesh3D(IndexedSeq(), TriangleList(IndexedSeq())))
        }
      }
    }
  }

  def load2(file: File): TriangleMesh[_3D] = {
//  def load(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = Try {

    val lines = Source.fromFile(file).getLines().toSeq

    // Extract header information
    val headerLines = lines.takeWhile(!_.startsWith("end_header"))
    val numVertices = headerLines.find(_.startsWith("element vertex")).map(_.split("\\s+")(2).toInt)
    val numFaces = headerLines.find(_.startsWith("element face")).map(_.split("\\s+")(2).toInt)
    val hasColors = headerLines.exists(_.startsWith("property uchar"))

    println((headerLines, numVertices, numFaces, hasColors))

    // Read vertices and faces
    val vertexLines = lines.slice(headerLines.length, headerLines.length + numVertices.getOrElse(0))
    val faceLines = lines.slice(headerLines.length + numVertices.getOrElse(0), lines.length)

    // Parse vertices
    val vertices: IndexedSeq[Point3D] = vertexLines.map { line =>
      val values = line.split("\\s+").map(_.toDouble)
      Point3D(values(0), values(1), values(2))
    }.toIndexedSeq

    // Parse faces
    val faces: IndexedSeq[TriangleCell] = faceLines.map { line =>
      val values = line.split("\\s+").map(_.toInt)
      TriangleCell(PointId(values(1)), PointId(values(2)), PointId(values(3)))
    }.toIndexedSeq

    // Create mesh based on header information
//    val mesh: Either[TriangleMesh[_3D], VertexColorMesh3D] = if (hasColors) {
//      // Create VertexColorMesh3D if colors are present
//      val colors = vertexLines.map { line =>
//        val values = line.split("\\s+").drop(3).map(_.toDouble)
//        RGB(values(0).toByte, values(1).toByte, values(2).toByte)
//      }
//      Right(VertexColorMesh3D(TriangleMesh3D(vertices, faces), colors))
//    } else {
//      // Create TriangleMesh if no colors
//      Left(TriangleMesh(vertices, faces))
//    }
//    mesh
    TriangleMesh3D(vertices, TriangleList(faces))
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
