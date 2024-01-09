package scalismo.io
import scalismo.common.PointId

import java.nio.charset.StandardCharsets
import scalismo.geometry.{_3D, Point3D}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh, TriangleMesh3D, VertexColorMesh3D}

import java.io.{
  BufferedOutputStream,
  BufferedReader,
  DataInputStream,
  DataOutputStream,
  File,
  FileInputStream,
  FileOutputStream,
  FileReader,
  IOException
}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object PLY {

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    val faceFile = new File("src/test/resources/facemesh.stl")
    val outTest = new File("src/test/resources/facemesh_test.ply")
    val outTest2 = new File("src/test/resources/facemesh_test2.ply")
    doSomeWriting(faceFile, outTest, outTest2)
    doSomeReading(outTest, outTest2)
  }

  def doSomeReading(input1: File, input2: File): Unit = {
    println("Reading stuff")
    val startTime = System.nanoTime()
    val m1: TriangleMesh[_3D] = MeshIO.readMesh(input1).get
    val midTime = System.nanoTime()
    val m2 = load(input2).get
    val endTime = System.nanoTime()
    val elapsedTimeSeconds0 = (midTime - startTime) / 1e9
    val elapsedTimeSeconds1 = (endTime - midTime) / 1e9
    println(s"Elapsed Time: $elapsedTimeSeconds0 seconds")
    println(s"Elapsed Time: $elapsedTimeSeconds1 seconds")
    println(m1 == m2)
  }

  def doSomeWriting(input: File, output1: File, output2: File): Unit = {
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

//  private def load(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = {

  private def load(file: File): Try[TriangleMesh[_3D]] = {

    // read the ply header to find out if the ply is a textured mesh in ASCII (in which case we return a failure since VTKPLYReader Update() would crash otherwise)
    // vtk loader: https://kitware.github.io/vtk-js/api/IO_Geometry_PLYReader.html
    if (!file.exists()) {
      val filename = file.getCanonicalFile
      Failure(new IOException(s"Could not read ply file with name $filename. Reason: The file does not exist"))
    } else {
      val breader = new BufferedReader(new FileReader(file))
      val lineIterator = Iterator.continually(breader.readLine())

      val headerLines = lineIterator.dropWhile(_ != "ply").takeWhile(_ != "end_header").toArray :+ "end_header"

      if (headerLines.exists(_.contains("TextureFile")) && headerLines.exists(_.contains("format ascii"))) {
        Failure(
          new IOException(
            "PLY file $filename seems to be a textured mesh in ASCII format which creates issues with the VTK ply reader. Please convert it to a binary ply or to a vertex color or shape only ply."
          )
        )
      } else {
        val headerInfo = parseHeader(headerLines).get
        val vertexInfo = headerInfo.elements.find(_.format == PLYElementFormat.VERTEX).get
        val vertexDefined = validateElementVertex(vertexInfo)
        val faceInfo = headerInfo.elements.find(_.format == PLYElementFormat.FACE)
        if (!vertexDefined.status) {
          Failure(
            new IOException(
              "Unsupported element property provided"
            )
          )
        } else if (!vertexDefined.is3DVertex) {
          Failure(
            new IOException(
              "Vertex (x,y,z) not defined in file."
            )
          )
        } else if (
          faceInfo.exists { case PLYElement(_, count, properties) =>
            count > 0 && properties.isEmpty
          }
        ) {
          Failure(
            new IOException(
              "Face element defined but no property defined."
            )
          )
        } else {
          if (headerInfo.format == PLYTypeFormat.ASCII) {
            Try { TriangleMesh3D(IndexedSeq(), TriangleList(IndexedSeq())) }
          } else {
            println("BINARY")
            val dis = new DataInputStream(new FileInputStream(file))
            dis.skipBytes(headerInfo.headerLength)
            val byteOrder =
              if (headerInfo.format.format == PLYTypeFormat.BINARY_LITTLE_ENDIAN) ByteOrder.LITTLE_ENDIAN
              else ByteOrder.BIG_ENDIAN

            val points = (0 until vertexInfo.count).map { i =>
              var x: Double = 0.0;
              var y: Double = 0.0;
              var z: Double = 0.0;
              var nx: Double = 0.0;
              var ny: Double = 0.0;
              var nz: Double = 0.0;
              var red: Int = 0;
              var green: Int = 0;
              var blue: Int = 0;
              var u: Double = 0.0
              var v: Double = 0.0
              vertexInfo.properties.foreach { item =>
                item.name.match {
                  case "x"     => x = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "y"     => y = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "z"     => z = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "nx"    => nx = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "ny"    => ny = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "nz"    => nz = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "red"   => red = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getInt
                  case "green" => green = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getInt
                  case "blue"  => blue = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getInt
                  case "u" | "s" | "texture_u" =>
                    u = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case "v" | "t" | "texture_v" =>
                    v = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getFloat
                  case _ => ??? // Throw error!!!
                }
              }
              Point3D(x, y, z)
            }
            val triangleList: Option[IndexedSeq[TriangleCell]] = faceInfo.map { faceInfo =>
              (0 until faceInfo.count).map { i =>
                val ids: Array[Int] = Array(0, 0, 0)
                faceInfo.properties.foreach { item =>
                  if (item.listFormat.isDefined) {
                    val cnt = readStream(dis, PLY_PROPERTY_ELEMENTS(item.listFormat.get), byteOrder).get.toInt
                    (0 until cnt).foreach { j =>
                      ids(j) = readStream(dis, PLY_PROPERTY_ELEMENTS(item.format), byteOrder).getInt
                    }
                  }
                }
                TriangleCell(PointId(ids(0)), PointId(ids(1)), PointId(ids(2)))
              }
            }

            Try {
              TriangleMesh3D(points, TriangleList(triangleList.getOrElse(IndexedSeq())))
            }
          }
        }
      }
    }
  }

  private def readStream(dis: DataInputStream, bytes: Int, byteOrder: ByteOrder): ByteBuffer = {
    val buffer = new Array[Byte](bytes)
    dis.readFully(buffer)
    ByteBuffer.wrap(buffer).order(byteOrder)
  }

  sealed trait PLYTypeFormat

  val PLY_FORMAT_ASCII = "ascii"
  val PLY_FORMAT_BIG = "binary_big_endian"
  val PLY_FORMAT_LITTLE = "binary_little_endian"

  val PLY_ELEMENT_VERTEX = "vertex"
  val PLY_ELEMENT_FACE = "face"

  private val PLY_PROPERTY_ELEMENTS = Map(
    "char" -> 1,
    "uchar" -> 1,
    "short" -> 2,
    "ushort" -> 2,
    "int" -> 4,
    "uint" -> 4,
    "float" -> 4,
    "double" -> 8
  )

  private object PLYTypeFormat extends Enumeration {
    type PLYTypeFormat = Value
    val ASCII, BINARY_BIG_ENDIAN, BINARY_LITTLE_ENDIAN = Value
  }

  private object PLYElementFormat extends Enumeration {
    type PLYElementFormat = Value
    val VERTEX, FACE = Value
  }

  private case class PLYFormat(format: PLYTypeFormat.PLYTypeFormat, version: String)
  private case class PLYProperty(format: String, name: String, listFormat: Option[String] = None)
  private case class PLYElement(format: PLYElementFormat.PLYElementFormat, count: Int, properties: Seq[PLYProperty])
  private case class HeaderInfo(format: PLYFormat, elements: Seq[PLYElement], comments: Seq[String], headerLength: Int)
  private case class PLYItemsDefined(status: Boolean,
                                     is3DVertex: Boolean,
                                     is3DNormal: Boolean,
                                     is3DVertexColor: Boolean,
                                     is3DUV: Boolean
  )

  private def validateElementVertex(element: PLYElement): PLYItemsDefined = {
    val xyz = Seq("x", "y", "z")
    val n = Seq("nx", "ny", "nz")
    val color = Seq("red", "green", "blue")
    val st = Seq("s", "t")
    val uv = Seq("u", "v")
    val texture = Seq("texture_u", "texture_v")
    val all = xyz ++ n ++ color ++ st ++ uv ++ texture
    val names = element.properties.map(_.name)
    val is3DVertexDefined = xyz.forall(names.contains)
    val is3DNormalDefined = n.forall(names.contains)
    val is3DVertexColorDefined = color.forall(names.contains)
    val is3DUVsDefined =
      st.forall(names.contains) ||
        uv.forall(names.contains) ||
        texture.forall(names.contains)
    val status = element.properties.map(_.name).forall(item => all.contains(item))
    PLYItemsDefined(status, is3DVertexDefined, is3DNormalDefined, is3DVertexColorDefined, is3DUVsDefined)
  }

  private def parseHeader(header: Array[String]): Try[HeaderInfo] = Try {
    val fileComments: ArrayBuffer[String] = ArrayBuffer.empty[String]
    var fileFormat: Option[PLYFormat] = None
    val fileElements: ArrayBuffer[PLYElement] = ArrayBuffer.empty[PLYElement]

    var cnt = 0
    while (cnt < header.length) {
      val line = header(cnt)
      val lineSplit = line.split(" ")
      val name = lineSplit.head
      if (name == "comment" || name == "obj_info") {
        fileComments += lineSplit.drop(1).mkString(" ")
      } else if (lineSplit.length > 2) {
        if (name == "format") {
          lineSplit(1) match {
            case PLY_FORMAT_ASCII =>
              fileFormat = Option(PLYFormat(PLYTypeFormat.ASCII, lineSplit(2)))
            case PLY_FORMAT_BIG =>
              fileFormat = Option(PLYFormat(PLYTypeFormat.BINARY_BIG_ENDIAN, lineSplit(2)))
            case PLY_FORMAT_LITTLE =>
              fileFormat = Option(PLYFormat(PLYTypeFormat.BINARY_LITTLE_ENDIAN, lineSplit(2)))
            case _ => Failure(new IOException(s"Unsupported PLY format"))
          }
        }
        if (name == "element") {
          val elementFormat = lineSplit(1) match {
            case PLY_ELEMENT_VERTEX =>
              Success(PLYElementFormat.VERTEX)
            case PLY_ELEMENT_FACE =>
              Success(PLYElementFormat.FACE)
            case _ => Failure(new IOException(s"Unsupported property format"))
          }
          val elementCount = lineSplit(2).toInt
          val elementProperties: ArrayBuffer[PLYProperty] = ArrayBuffer.empty[PLYProperty]
          while (cnt < header.length - 1 && (header(cnt + 1).split(" ").head) == "property") {
            val nextLine = header(cnt + 1).split(" ")
            if (nextLine(1) == "list") {
              elementProperties += PLYProperty(nextLine(3), nextLine(4), Option(nextLine(2)))
            } else {
              elementProperties += PLYProperty(nextLine(1), nextLine(2))
            }
            cnt += 1
          }
          fileElements += PLYElement(elementFormat.get, elementCount, elementProperties.toArray)
        }
      }
      cnt += 1
    }
    HeaderInfo(fileFormat.get,
               fileElements.toSeq,
               fileComments.toSeq,
               header.map(_.getBytes("UTF-8").length).sum + header.length
    )
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
