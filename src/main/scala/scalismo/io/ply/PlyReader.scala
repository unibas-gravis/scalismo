/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package scalismo.io.ply

import java.io.{BufferedInputStream, File, FileInputStream, IOException}
import java.nio.ByteOrder
import java.nio.file.{Files, Path, Paths}
import java.util.{Locale, Scanner}

import scalismo.color.RGBA

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
 * Reads a ply file based on its header. This class is independent of the mesh class and could be reused in other
 * ply-readers.
 */
object PlyReader {

  import PlyHelpers._

  /**
   * Read the ply file from the url. Returns a list of textures and a list of elements, where each element is a list of
   * properties. This reflects the structure of elements and properties from the header.
   *
   * @param url
   *   File to read.
   * @return
   */
  def read(url: String): List[(String, List[(String, List[_])])] = {
    val lineSeparatorSize = getLineSeparatorSize(url)

    val path = Paths.get(url)
    val plyFile = path.getFileName
    val dir = path.getParent

    val header = readHeader(url)
    checkHeader(header)

    val plyFormat = parsePlyFormat(header)
    val elementReaders = parseElementsInHeader(header)

    // as we use buffered readers we need to reset the input stream
    val fis = new FileInputStream(url)
    val bis = new BufferedInputStream(fis)
    bis.skip(header.map(_.size).sum + header.size * lineSeparatorSize)
    val values = readData(bis, plyFormat, elementReaders)
    values
  }

  private def readData(bis: BufferedInputStream,
                       plyFormat: PlyFormat.PlyFormat,
                       elementReaders: List[(String, PlyElementReader)]
  ): List[(String, List[(String, List[_])])] = {
    plyFormat match {
      case PlyFormat.ASCII =>
        def readFormat(locale: Locale) = Try {
          val scanner = new Scanner(bis).useLocale(locale)
          elementReaders.map { reader =>
            (reader._1, reader._2.read(scanner))
          }
        }
        // read floating point number either with point or comma
        readFormat(java.util.Locale.US).getOrElse(readFormat(java.util.Locale.FRENCH).get)
      case PlyFormat.BinaryLittleEndian =>
        elementReaders.map { reader =>
          (reader._1, reader._2.read(bis, ByteOrder.LITTLE_ENDIAN))
        }
      case PlyFormat.BinaryBigEndian =>
        elementReaders.map { reader =>
          (reader._1, reader._2.read(bis, ByteOrder.BIG_ENDIAN))
        }
      case _ =>
        throw new IOException("Can not read ply format \"%s\".".format(plyFormat))
    }
  }

  private def readHeader(url: String): List[String] = {
    val fis = new FileInputStream(url)
    val bis = new BufferedInputStream(fis)
    val scanner = new Scanner(bis)
    val lines = new ListBuffer[String]()
    while (scanner.hasNextLine) {
      val line = scanner.nextLine()
      if (line != PLY.endHeader) {
        lines += line
      } else {
        lines += line
        return lines.toList
      }
    }
    throw new IOException("Could not read until \"" + PLY.endHeader + "\".")
  }

  private def checkHeader(header: List[String]) = {

    if (header.head.trim != PLY.startHeader) {
      throw new IOException("Header does not start with \"" + PLY.startHeader + "\" as expected for ply files.")
    }

    if (header.reverse.head.trim != PLY.endHeader) {
      throw new IOException("Header does not end with \"" + PLY.endHeader + "\" as expected for ply files.")
    }

  }

  private def parsePlyFormat(header: List[String]) = {

    val linesWithFormat = header.filter(s => s.startsWith(PLY.format))
    if (linesWithFormat.size != 1) {
      throw new IOException("Header does not have exactly one line specifying the file format.")
    }

    val formatLine = linesWithFormat.head
    val parts = formatLine.split(" ")
    if (parts.size != 3) {
      throw new IOException("Exepected format line to consist of three parts: \"format <FORMAT_TYPE> <VERSION_NBR>\"")
    }

    val format = PlyFormat.values.find(v => v.toString == parts(1))
    if (format.isEmpty) {
      throw new IOException("Can not read ply file in the format " + parts(1) + ".")
    }
    format.get
  }

  private def parseElementsInHeader(header: List[String]): List[(String, PlyElementReader)] = {
    val startingElement = header.dropWhile(l => !l.startsWith(PLY.element))
    val elements = parseElementFromListStart(startingElement)
    elements.flatMap(headerPartToReader)
  }

  private def parseElementFromListStart(list: List[String]): List[List[String]] = {
    if (list.isEmpty || list.head == PLY.endHeader) return Nil
    val eleStart = list.head
    val (eleBody, tailList) = list.tail.span(s => !s.startsWith(PLY.element) && !s.startsWith(PLY.endHeader))
    (eleStart :: eleBody) :: parseElementFromListStart(tailList)
  }

  private def headerPartToReader(list: List[String]): Option[(String, PlyElementReader)] = {

    val firstLine = list.head.split(" ")

    assert(
      firstLine.size == 3,
      "Expect that the starting line of an element in the header has the form: \"" + PLY.element + " <TYPE> <NUMBER>\""
    )
    assert(firstLine(0) == "element", "The first tag of the element line should be \"" + PLY.element + "\".")

    val elementType = firstLine(1)
    val N = firstLine(2).toInt

    elementType match {
      case e if e.equals(PLY.vertex) || e.equals(PLY.face) =>
        Some((e, PlyElementReader(N, parsePropertyHeader(list.tail))))
      case _ =>
        System.err.println("IGNORING element of type \"" + elementType + "\" as its case is not handled.")
        None
    }
  }

  private def parsePropertyHeader(list: List[String]): List[(String, PlyPropertyReader[_])] = {
    val partList = list.map(_.split(" "))
    assert(partList.forall(p => p(0) == PLY.property),
           "Expect that the property of an element starts with \"" + PLY.property + "\"."
    )
    assert(partList.forall(p => p.size >= 3), "Expect that the properties consists of at least three elements.")
    val parts = partList.map(p => (p.last, p.init))

    parts.map { p =>
      val reader = makeReader(p._2.tail)
      (p._1, reader)
    }
  }

  private def makeReader(format: Array[String]): PlyPropertyReader[_] = {
    if (format.size == 1) {
      PlyTypes.withName(format(0)) match {
        case PlyTypes.char | PlyTypes.int8                 => new PlyPropertyReader(new FixedLengthSequenceReader[Char])
        case PlyTypes.uchar | PlyTypes.uint8               => new PlyPropertyReader(new FixedLengthSequenceReader[Byte])
        case PlyTypes.int | PlyTypes.int32 | PlyTypes.uint => new PlyPropertyReader(new FixedLengthSequenceReader[Int])
        case PlyTypes.long | PlyTypes.int64                => new PlyPropertyReader(new FixedLengthSequenceReader[Long])
        case PlyTypes.float | PlyTypes.float32 | PlyTypes.float32 =>
          new PlyPropertyReader(new FixedLengthSequenceReader[Float])
        case PlyTypes.double | PlyTypes.float64 => new PlyPropertyReader(new FixedLengthSequenceReader[Double])
        case PlyTypes.short | PlyTypes.int16 | PlyTypes.ushort =>
          new PlyPropertyReader(new FixedLengthSequenceReader[Short])
        case _ =>
          throw new IOException("Do not know how to read property type: " + format(0))
      }
    } else if (format.size == 3) {
      PlyTypes.withName(format(2)) match {
        case PlyTypes.char | PlyTypes.int8   => new PlyPropertyReader(new VariableLengthSequenceReader[Char])
        case PlyTypes.uchar | PlyTypes.uint8 => new PlyPropertyReader(new VariableLengthSequenceReader[Byte])
        case PlyTypes.int | PlyTypes.int32 | PlyTypes.uint =>
          new PlyPropertyReader(new VariableLengthSequenceReader[Int])
        case PlyTypes.long | PlyTypes.int64     => new PlyPropertyReader(new VariableLengthSequenceReader[Long])
        case PlyTypes.float | PlyTypes.float32  => new PlyPropertyReader(new VariableLengthSequenceReader[Float])
        case PlyTypes.double | PlyTypes.float64 => new PlyPropertyReader(new VariableLengthSequenceReader[Double])
        case PlyTypes.short | PlyTypes.int16 | PlyTypes.ushort =>
          new PlyPropertyReader(new VariableLengthSequenceReader[Short])
        case _ =>
          throw new IOException("Do not know how to read property type: " + format.mkString(" "))
      }
    } else {
      throw new IOException("Do not know how to read property type: " + format.mkString(" "))
    }
  }

  private def getLineSeparatorSize(url: String): Int = {
    val fis = new FileInputStream(url)
    while (fis.available() > 0) {
      val c = fis.read().asInstanceOf[Char]
      if ((c == '\n') || (c == '\r')) {
        if (fis.available() > 0) {
          val c2 = fis.read().asInstanceOf[Char]
          if (((c2 == '\n') || (c2 == '\r')) && c != c2) return 2
          return 1
        }
        return 1
      }
      if (c == '\u0085' || c == '\u2028' || c == '\u2029') {
        return 1
      }
    }
    // default line separator size is 1, this will be choosen if file is empty or does not use \r or \n characters (will use unicode characters)
    return 1
  }

}
