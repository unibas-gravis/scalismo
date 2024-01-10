package scalismo.io
import scalismo.color.RGBA
import scalismo.common.PointId
import scalismo.geometry.{Point3D, _3D}
import scalismo.mesh.*

import java.io.*
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object PLY {

  val PLY_FORMAT_ASCII = "ascii"
  val PLY_FORMAT_BIG = "binary_big_endian"
  val PLY_FORMAT_LITTLE = "binary_little_endian"
  val PLY_ELEMENT_VERTEX = "vertex"
  val PLY_ELEMENT_FACE = "face"

  def save(surface: Either[TriangleMesh[_3D], VertexColorMesh3D], file: File): Try[Unit] = {
    val (mesh, colors) = surface match {
      case Right(colorMesh) => (colorMesh.shape, Option(colorMesh.color.pointData.iterator))
      case Left(shapeOnly)  => (shapeOnly, None)
    }
    val hasColor = colors.isDefined

    val headerContent = createHeader(mesh.pointSet.numberOfPoints, mesh.triangulation.triangles.length, hasColor)

    Try {
      val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
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

  private def createHeader(numVertices: Int, numFaces: Int, vertexColors: Boolean): String = {
    val header = new StringBuilder
    header.append("ply\nformat binary_little_endian 1.0\ncomment Scalismo generated PLY File\n")
    header.append(f"element vertex $numVertices\nproperty float x\nproperty float y\nproperty float z\n")
    if (vertexColors) {
      header.append("property uchar red\nproperty uchar green\nproperty uchar blue\nproperty uchar alpha\n")
    }
    header.append(f"element face $numFaces\nproperty list uchar int vertex_indices\nend_header\n")
    header.toString()
  }

  def load(file: File): Try[Either[TriangleMesh[_3D], VertexColorMesh3D]] = {
    if (!file.exists()) {
      val filename = file.getCanonicalFile
      Failure(new IOException(s"Could not read ply file with name $filename. Reason: The file does not exist"))
    } else {
      Try {
        val breader = new BufferedReader(new FileReader(file))
        val lineIterator = Iterator.continually(breader.readLine())
        val headerLines = lineIterator.dropWhile(_ != "ply").takeWhile(_ != "end_header").toArray :+ "end_header"

        val headerInfo: HeaderInfo = parseHeader(headerLines).get
        val (points, colors, triangles) = if (headerInfo.format.format == PLYTypeFormat.ASCII) {
          val (points, colors) = readPoints(lineIterator, headerInfo.vertexInfo)
          val triangles = readTriangles(lineIterator, headerInfo.faceInfo)
          (points, colors, triangles)
        } else {
          val byteOrder =
            if (headerInfo.format.format == PLYTypeFormat.BINARY_LITTLE_ENDIAN) ByteOrder.LITTLE_ENDIAN
            else ByteOrder.BIG_ENDIAN

          val dataBuffer = readFileToByteBuffer(file.toString)
          dataBuffer.position(headerInfo.headerLength)
          dataBuffer.slice().order(byteOrder)

          val (points, colors) = readPointsBinary(dataBuffer, headerInfo.vertexInfo, byteOrder)
          val triangles = readTrianglesBinary(dataBuffer, headerInfo.faceInfo, byteOrder)
          (points, colors, triangles)
        }
        val mesh = TriangleMesh3D(points, TriangleList(triangles))
        colors match {
          case Some(c) => Right(VertexColorMesh3D(mesh, SurfacePointProperty[RGBA](mesh.triangulation, c)))
          case None    => Left(mesh)
        }
      }
    }
  }

  private def readFileToByteBuffer(file: String): ByteBuffer = {
    val raf = new RandomAccessFile(file, "r")
    val channel = raf.getChannel
    val buffer = ByteBuffer.allocate(channel.size.toInt)
    channel.read(buffer)
    buffer.rewind()
    buffer
  }

  private def getPropertyIndex(items: Seq[PLYProperty], name: String): Option[Int] = {
    items.indexWhere(_.name == name) match {
      case -1    => None
      case index => Some(index)
    }
  }

  private def getPointPropertyIndex(items: Seq[PLYProperty]): PropertyIndexes = {
    val xIndex = getPropertyIndex(items, "x").get
    val yIndex = getPropertyIndex(items, "y").get
    val zIndex = getPropertyIndex(items, "z").get

    val redIndex = getPropertyIndex(items, "red")
    val greenIndex = getPropertyIndex(items, "green")
    val blueIndex = getPropertyIndex(items, "blue")
    val alphaIndex = getPropertyIndex(items, "alpha")

    val isRGB = redIndex.isDefined && greenIndex.isDefined && blueIndex.isDefined
    val isRGBA = isRGB && alphaIndex.isDefined
    PropertyIndexes(xIndex, yIndex, zIndex, redIndex, greenIndex, blueIndex, alphaIndex, isRGB, isRGBA)
  }

  private def readPoints(lineIterator: Iterator[String],
                         vertexInfo: PLYElement
  ): (IndexedSeq[Point3D], Option[IndexedSeq[RGBA]]) = {
    val pIndex = getPointPropertyIndex(vertexInfo.properties)
    val data: IndexedSeq[(Point3D, Option[RGBA])] = (0 until vertexInfo.count).map { _ =>
      val line = lineIterator.next()
      val split = line.split(" ")
      val p = Point3D(
        split(pIndex.x).toFloat,
        split(pIndex.y).toFloat,
        split(pIndex.z).toFloat
      )
      val c = if (pIndex.isRGB) {
        Option(
          RGBA(
            split(pIndex.red.get).toDouble / 255.0,
            split(pIndex.green.get).toDouble / 255.0,
            split(pIndex.blue.get).toDouble / 255.0,
            1.0
          )
        )
      } else if (pIndex.isRGBA) {
        Option(
          RGBA(
            split(pIndex.red.get).toDouble / 255.0,
            split(pIndex.green.get).toDouble / 255.0,
            split(pIndex.blue.get).toDouble / 255.0,
            split(pIndex.alpha.get).toDouble / 255.0
          )
        )
      } else {
        None
      }
      (p, c)
    }
    val (points, colors) = data.unzip
    val flattenedColors: Option[IndexedSeq[RGBA]] = if (colors.exists(_.isEmpty)) None else Some(colors.flatten)
    (points, flattenedColors)
  }

  private def readPointsBinary(buffer: ByteBuffer,
                               vertexInfo: PLYElement,
                               byteOrder: ByteOrder
  ): (IndexedSeq[Point3D], Option[IndexedSeq[RGBA]]) = {
    val pIndex = getPointPropertyIndex(vertexInfo.properties)
    val data: IndexedSeq[(Point3D, Option[RGBA])] = (0 until vertexInfo.count).map { _ =>
      val data = vertexInfo.properties.map { item =>
        val value = readProperty(buffer, item.format, byteOrder)
        (item.name, value)
      }
      val p = Point3D(
        data(pIndex.x)._2.asInstanceOf[Float],
        data(pIndex.y)._2.asInstanceOf[Float],
        data(pIndex.z)._2.asInstanceOf[Float]
      )
      val c = if (pIndex.isRGB) {
        Option(
          RGBA(
            data(pIndex.red.get)._2.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.green.get)._2.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.blue.get)._2.asInstanceOf[Int].toDouble / 255.0,
            1.0
          )
        )
      } else if (pIndex.isRGBA) {
        Option(
          RGBA(
            data(pIndex.red.get)._2.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.green.get)._2.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.blue.get)._2.asInstanceOf[Int].toDouble / 255.0,
            data(pIndex.alpha.get)._2.asInstanceOf[Int].toDouble / 255.0
          )
        )
      } else {
        None
      }
      (p, c)
    }
    val (points, colors) = data.unzip
    val flattenedColors: Option[IndexedSeq[RGBA]] = if (colors.exists(_.isEmpty)) None else Some(colors.flatten)
    (points, flattenedColors)
  }

  private def readTriangles(lineIterator: Iterator[String], faceInfo: PLYElement): IndexedSeq[TriangleCell] = {
    if (faceInfo.count == 0) {
      IndexedSeq.empty
    } else {
      // TODO: Update to support texture file
      faceInfo.properties.headOption
        .map { propertyList =>
          val listCounterFormat = propertyList.listFormat.get
          val listFormat = propertyList.format
          (0 until faceInfo.count).map { _ =>
            val line = lineIterator.next()
            val split = line.split(" ")
            val cnt = split(0).toInt
            if (cnt != 3) {
              throw new IOException(
                "Faces elements different than 3 specified."
              )
            }
            (1 to 3).map { i =>
              split(i).toInt
            }
          }
        }
        .getOrElse(IndexedSeq.empty)
        .map { case Seq(id1, id2, id3) =>
          TriangleCell(PointId(id1), PointId(id2), PointId(id3))
        }
    }
  }

  private def readTrianglesBinary(buffer: ByteBuffer,
                                  faceInfo: PLYElement,
                                  byteOrder: ByteOrder
  ): IndexedSeq[TriangleCell] = {
    if (faceInfo.count == 0) {
      IndexedSeq.empty
    } else {
      faceInfo.properties.headOption
        .map { propertyList =>
          val listCounterFormat = propertyList.listFormat.get
          val listFormat = propertyList.format
          (0 until faceInfo.count).map { _ =>
            val cnt = readProperty(buffer, listCounterFormat, byteOrder).asInstanceOf[Int]
            if (cnt != 3) {
              throw new IOException(
                "Faces elements different than 3 specified."
              )
            }
            (0 until 3).map { _ =>
              readProperty(buffer, listFormat, byteOrder).asInstanceOf[Int]
            }
          }
        }
        .getOrElse(IndexedSeq.empty)
        .map { case Seq(id1, id2, id3) =>
          TriangleCell(PointId(id1), PointId(id2), PointId(id3))
        }
    }
  }

  private def readProperty(buffer: ByteBuffer, format: String, byteOrder: ByteOrder): Any = {
    format match {
      case "char"   => buffer.order(byteOrder).get()
      case "uchar"  => buffer.order(byteOrder).get() & 0xff
      case "short"  => buffer.order(byteOrder).getShort
      case "ushort" => buffer.order(byteOrder).getShort & 0xffff
      case "int"    => buffer.order(byteOrder).getInt
      case "uint"   => buffer.order(byteOrder).getInt & 0xffffffff
      case "float"  => buffer.order(byteOrder).getFloat
      case "double" => buffer.order(byteOrder).getDouble
    }
  }

  private def validateElementFace(element: PLYElement): Boolean = {
    if (element.count == 0) true
    else {
      element.properties.length == 1 && element.properties.head.listFormat.isDefined && Seq(
        "vertex_indices",
        "vertex_index"
      ).contains(element.properties.head.name)
    }
  }

  private def validateElementVertex(element: PLYElement): PLYItemsDefined = {
    val xyz = Seq("x", "y", "z")
    val n = Seq("nx", "ny", "nz")
    val color = Seq("red", "green", "blue")
    val colora = Seq("red", "green", "blue", "alpha")
    val st = Seq("s", "t")
    val uv = Seq("u", "v")
    val texture = Seq("texture_u", "texture_v")
    val all = xyz ++ n ++ color ++ colora ++ st ++ uv ++ texture
    val names = element.properties.map(_.name)
    val is3DVertexDefined = xyz.forall(names.contains)
    val is3DNormalDefined = n.forall(names.contains)
    val is3DVertexColorDefined = color.forall(names.contains) || colora.forall(names.contains)
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

    val vertexInfo = fileElements.find(_.format == PLYElementFormat.VERTEX).get
    val faceInfo = fileElements.find(_.format == PLYElementFormat.FACE).get
    val faceDefined = validateElementFace(faceInfo)
    val vertexDefined = validateElementVertex(vertexInfo)
    if (!vertexDefined.status) {
      throw new IOException(
        "Unsupported element property provided"
      )
    } else if (!vertexDefined.is3DVertex) {
      throw new IOException(
        "Vertex (x,y,z) not defined in file."
      )
    } else if (!faceDefined) {
      throw new IOException(
        "Face element defined but no property defined."
      )
    }
    HeaderInfo(
      format = fileFormat.get,
      vertexInfo = vertexInfo,
      faceInfo = faceInfo,
      comments = fileComments.toArray,
      headerLength = header.map(_.getBytes("UTF-8").length).sum + header.length
    )
  }

  sealed trait PLYTypeFormat

  case class PropertyIndexes(x: Int,
                             y: Int,
                             z: Int,
                             red: Option[Int],
                             green: Option[Int],
                             blue: Option[Int],
                             alpha: Option[Int],
                             isRGB: Boolean,
                             isRGBA: Boolean
  )

  private case class PLYFormat(format: PLYTypeFormat.PLYTypeFormat, version: String)

  private case class PLYProperty(format: String, name: String, listFormat: Option[String] = None)

  private case class PLYElement(format: PLYElementFormat.PLYElementFormat, count: Int, properties: Seq[PLYProperty])

  private case class HeaderInfo(format: PLYFormat,
                                vertexInfo: PLYElement,
                                faceInfo: PLYElement,
                                comments: Seq[String],
                                headerLength: Int
  )

  private case class PLYItemsDefined(status: Boolean,
                                     is3DVertex: Boolean,
                                     is3DNormal: Boolean,
                                     is3DVertexColor: Boolean,
                                     is3DUV: Boolean
  )

  private object PLYTypeFormat extends Enumeration {
    type PLYTypeFormat = Value
    val ASCII, BINARY_BIG_ENDIAN, BINARY_LITTLE_ENDIAN = Value
  }

  private object PLYElementFormat extends Enumeration {
    type PLYElementFormat = Value
    val VERTEX, FACE = Value
  }

}
