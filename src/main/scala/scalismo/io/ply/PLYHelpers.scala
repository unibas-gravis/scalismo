package scalismo.io.ply

import java.io.IOException
import java.nio.{ByteBuffer, ByteOrder}

val PLY_FORMAT_ASCII = "ascii"
val PLY_FORMAT_BIG = "binary_big_endian"
val PLY_FORMAT_LITTLE = "binary_little_endian"
val PLY_ELEMENT_VERTEX = "vertex"
val PLY_ELEMENT_FACE = "face"
val VERTEX_INDEX_PROPERTY_NAMES = Seq(
  "vertex_indices",
  "vertex_index"
)

sealed trait PLYTypeFormat

case class PropertyVertexIndexes(x: Int,
                                 y: Int,
                                 z: Int,
                                 red: Option[Int],
                                 green: Option[Int],
                                 blue: Option[Int],
                                 alpha: Option[Int],
                                 isRGB: Boolean,
                                 isRGBA: Boolean
)

case class PropertyFaceIndexes(vertexIndex: Option[Int])

case class PLYFormat(format: PLYTypeFormat.PLYTypeFormat, version: String)

case class PLYProperty(format: String, name: String, listFormat: Option[String] = None) {
  val isList: Boolean = listFormat.isDefined
  private def readItem(formatType: String, buffer: ByteBuffer, byteOrder: ByteOrder): Any = {
    formatType match {
      case "char"   => buffer.order(byteOrder).get()
      case "uchar"  => buffer.order(byteOrder).get() & 0xff
      case "short"  => buffer.order(byteOrder).getShort
      case "ushort" => buffer.order(byteOrder).getShort & 0xffff
      case "int"    => buffer.order(byteOrder).getInt
      case "uint"   => buffer.order(byteOrder).getInt & 0xffffffff
      case "float"  => buffer.order(byteOrder).getFloat
      case "double" => buffer.order(byteOrder).getDouble
      case _ =>
        throw new IOException(
          s"Format type ${formatType} not supported."
        )
    }
  }
  private def readList(buffer: ByteBuffer, byteOrder: ByteOrder): Seq[Any] = {
    val listLength = readItem(format, buffer, byteOrder).asInstanceOf[Int]
    (0 until listLength).map(_ => readItem(listFormat.get, buffer, byteOrder))
  }

  def read(buffer: ByteBuffer, byteOrder: ByteOrder): Seq[Any] = {
    if (isList) readList(buffer, byteOrder) else Array(readItem(format, buffer, byteOrder))
  }

}

case class PLYElement(format: PLYElementFormat.PLYElementFormat, count: Int, properties: Seq[PLYProperty])

case class HeaderInfo(format: PLYFormat,
                      vertexInfo: PLYElement,
                      faceInfo: PLYElement,
                      comments: Seq[String],
                      headerLength: Int
)

case class PLYItemsDefined(status: Boolean,
                           is3DVertex: Boolean,
                           is3DNormal: Boolean,
                           is3DVertexColor: Boolean,
                           is3DUV: Boolean
)

object PLYTypeFormat extends Enumeration {
  type PLYTypeFormat = Value
  val ASCII, BINARY_BIG_ENDIAN, BINARY_LITTLE_ENDIAN = Value
}

object PLYElementFormat extends Enumeration {
  type PLYElementFormat = Value
  val VERTEX, FACE = Value
}

object PLYHelpers {

  private def getPropertyIndex(items: Seq[PLYProperty], name: String): Option[Int] = {
    items.indexWhere(_.name == name) match {
      case -1    => None
      case index => Some(index)
    }
  }

  def getVertexPropertyIndex(items: Seq[PLYProperty]): PropertyVertexIndexes = {
    val xIndex = getPropertyIndex(items, "x").get
    val yIndex = getPropertyIndex(items, "y").get
    val zIndex = getPropertyIndex(items, "z").get

    val redIndex = getPropertyIndex(items, "red")
    val greenIndex = getPropertyIndex(items, "green")
    val blueIndex = getPropertyIndex(items, "blue")
    val alphaIndex = getPropertyIndex(items, "alpha")

    val isRGB = redIndex.isDefined && greenIndex.isDefined && blueIndex.isDefined
    val isRGBA = isRGB && alphaIndex.isDefined
    PropertyVertexIndexes(xIndex, yIndex, zIndex, redIndex, greenIndex, blueIndex, alphaIndex, isRGB, isRGBA)
  }

  def getFacePropertyIndex(items: Seq[PLYProperty]): PropertyFaceIndexes = {
    val faceIndex: Option[Int] = VERTEX_INDEX_PROPERTY_NAMES.collectFirst { f =>
      getPropertyIndex(items, f)
    }.flatten
    PropertyFaceIndexes(faceIndex)
  }

}
