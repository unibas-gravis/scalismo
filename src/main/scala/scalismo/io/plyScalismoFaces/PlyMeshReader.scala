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
package scalismo.io.plyScalismoFaces

import java.io._
import java.nio.ByteOrder
import java.util.Scanner

import scalismo.color.RGBA
import scalismo.geometry._
import scalismo.mesh._

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/**
 * Function that try to parse certain mesh properties from the data read of a ply file by the PlyReader.
 */
object PlyMeshReader {

  import PlyHelpers._
  import PlyMeshHelper._

  def getProperties(
    values: List[(String, List[(String, List[_])])]
  ): (List[(String, List[_])], List[(String, List[_])]) = {
    (getVertexProperties(values), getFaceProperties(values))
  }

  def getFaceProperties(values: List[(String, List[(String, List[_])])]): List[(String, List[_])] = {
    values
      .find(e => e._1 == PLY.face)
      .getOrElse(
        throw new IOException("Could not read face properties.")
      )
      ._2
  }

  def getVertexProperties(values: List[(String, List[(String, List[_])])]): List[(String, List[_])] = {
    values
      .find(e => e._1 == PLY.vertex)
      .getOrElse(
        throw new IOException("Could not read vertex properties.")
      )
      ._2
  }

  def getVertices(values: List[(String, List[_])]): IndexedSeq[Point[_3D]] = {
    val x = values
      .find(e => e._1 == PLY.xCoordinate)
      .getOrElse(
        throw new IOException("Could not read x coordinates.")
      )
      ._2
      .map(x => any2Double(x))

    val y = values
      .find(e => e._1 == PLY.yCoordinate)
      .getOrElse(
        throw new IOException("Could not read y coordinates.")
      )
      ._2
      .map(x => any2Double(x))

    val z = values
      .find(e => e._1 == PLY.zCoordinate)
      .getOrElse(
        throw new IOException("Could not read z coordinates.")
      )
      ._2
      .map(x => any2Double(x))

    CollectionTools.zip3(x, y, z).map(t => Point(t._1, t._2, t._3)).toIndexedSeq
  }

  def getTriangles(values: List[(String, List[_])]): TriangleList = {
    val indices = values
      .find(e => e._1 == PLY.vertexIndices)
      .getOrElse(
        throw new IOException("Could not read vertex indices.")
      )
      ._2
      .grouped(3)
      .map(a => listOfAny2ListOfPointIDs(a))
    TriangleList(indices.map(l => TriangleCell(l(0), l(1), l(2))).toIndexedSeq)
  }

  def getColors(values: List[(String, List[_])]): IndexedSeq[RGBA] = {
    val reds: Seq[Double] = values
      .find(e => e._1 == PLY.red)
      .getOrElse(
        throw new IOException("Could not read red color channel.")
      )
      ._2
      .map(x => byte2ZeroOne(any2Byte(x)))

    val greens: Seq[Double] = values
      .find(e => e._1 == PLY.green)
      .getOrElse(
        throw new IOException("Could not read green color channel.")
      )
      ._2
      .map(x => byte2ZeroOne(any2Byte(x)))

    val blues: Seq[Double] = values
      .find(e => e._1 == PLY.blue)
      .getOrElse(
        throw new IOException("Could not read blue color channel.")
      )
      ._2
      .map(x => byte2ZeroOne(any2Byte(x)))

    val alphas: Option[Seq[Double]] = values.find(e => e._1 == PLY.alpha).map {
      _._2.map(x => byte2ZeroOne(any2Byte(x)))
    }

    alphas match {
      case Some(alphaValues) => // with alpha channel
        CollectionTools
          .zip3(reds, greens, blues)
          .zip(alphaValues)
          .map { case ((r, g, b), a) => RGBA(r, g, b, a) }
          .toIndexedSeq
      case None => // RGB only
        CollectionTools.zip3(reds, greens, blues).map { case (r, g, b) => RGBA(r, g, b) }.toIndexedSeq
    }
  }
}

case class PlyElementReader(N: Int, readers: List[(String, PlyPropertyReader[_])]) {

  def read(is: InputStream, bo: ByteOrder): List[(String, List[_])] = {
    val readerList = readers.map(_._2)
    for (i <- 0 until N) {
      readerList.foreach(r => r.read(is, bo))
    }
    readers.map(e => (e._1, e._2.getList))
  }

  def read(scanner: Scanner): List[(String, List[_])] = {
    for (i <- 0 until N) {
      readers.foreach(r => r._2.read(scanner))
    }
    readers.map(e => (e._1, e._2.getList))
  }

}

class PlyPropertyReader[A](private val reader: SequenceReader[A]) {

  private val _buffer: ListBuffer[A] = new ListBuffer[A]

  def getList: List[A] = _buffer.toList

  def read(scanner: Scanner): Unit = {
    _buffer ++= reader.read(scanner)
  }

  def read(is: InputStream, bo: ByteOrder): Unit = {
    _buffer ++= reader.read(is, bo)
  }

}
