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

import java.io.{IOException, InputStream}
import java.nio._
import java.nio.channels._
import java.util.Scanner

/**
 * General trait to handle different sequence/list readers
 */
trait SequenceReader[@specialized(Byte, Char, Short, Int, Long, Float, Double) A] {

  /** read ascii text format */
  def read(scanner: Scanner): Seq[A]

  /** read binary format with given byte order */
  def read(is: InputStream, bo: ByteOrder): Seq[A]
}

/**
 * The SequenceReader reads a predefined number of elements of type `A`. The number of elements can be passed when
 * constructing the reader or in the read call.
 *
 * ASCII/String example for A:Double "1.0 2.0 3.0 4.0 5.0 ..." => read(2,scanner) reads 2 elements of type A and returns
 * then Seq(1.0,2.0)
 *
 * @param n
 *   Number of elements to be read.
 * @tparam A
 *   ElementType of the sequence to be read.
 */
class FixedLengthSequenceReader[
  @specialized(Byte, Char, Short, Int, Long, Float, Double) A: StringReader: EndianReader
](private val n: Int = 1)
    extends SequenceReader[A] {

  override def read(scanner: Scanner): Seq[A] = {
    read(n, scanner)
  }

  override def read(is: InputStream, bo: ByteOrder): Seq[A] = {
    read(n, is, bo)
  }

  def read(N: Int, scanner: Scanner): Seq[A] = {
    implicitly[StringReader[A]].read(N, scanner)
  }

  def read(N: Int, is: InputStream, bo: ByteOrder): Seq[A] = {
    implicitly[EndianReader[A]].read(N, is, bo)
  }
}

/**
 * The ListReader reads first the number of element to read and then the sequence of `A`s. The class uses a
 * `SequenceReader` once the number of elements to read is determined.
 *
 * @note
 *   The number of elements is limited to be of type `Byte`.
 *
 * ASCII/String example for A:Double "3 1.0 1.0 1.0 2.0 4.0 6.0 ....." => reads n=3 then reads and returns
 * Seq(1.0,1.0,1.0)
 * @tparam A
 *   ElementType of the sequence to be read.
 */
class VariableLengthSequenceReader[
  @specialized(Byte, Char, Short, Int, Long, Float, Double) A: StringReader: EndianReader
] extends SequenceReader[A] {

  val fixedReader = new FixedLengthSequenceReader[A]()

  override def read(scanner: Scanner): Seq[A] = {
    val n = scanner.nextInt()
    fixedReader.read(n, scanner)
  }

  override def read(is: InputStream, bo: ByteOrder): Seq[A] = {
    val n = 1
    val buffer = new Array[Byte](n)
    val bytesRead = is.read(buffer)
    if (bytesRead != n) {
      throw new IOException("Unexpected End of Stream while reading the length of a list.")
    }
    val N = ByteBuffer.wrap(buffer).order(bo).get
    fixedReader.read(N, is, bo)
  }
}

trait StringReader[@specialized(Byte, Char, Short, Int, Long, Float, Double) A] {
  def read(N: Int, scanner: Scanner): Seq[A]
}

object StringReader {

  implicit object ByteStringReader extends StringReader[Byte] {
    def read(N: Int, scanner: Scanner): Seq[Byte] = {
      for (i <- 0 until N) yield scanner.nextInt.toByte
    }
  }

  implicit object CharStringReader extends StringReader[Char] {
    def read(N: Int, scanner: Scanner): Seq[Char] = {
      for (i <- 0 until N) yield scanner.nextInt.toChar
    }
  }

  implicit object ShortStringReader extends StringReader[Short] {
    def read(N: Int, scanner: Scanner): Seq[Short] = {
      for (i <- 0 until N) yield scanner.nextShort
    }
  }

  implicit object IntStringReader extends StringReader[Int] {
    def read(N: Int, scanner: Scanner): Seq[Int] = {
      for (i <- 0 until N) yield scanner.nextInt
    }
  }

  implicit object LongStringReader extends StringReader[Long] {
    def read(N: Int, scanner: Scanner): Seq[Long] = {
      for (i <- 0 until N) yield scanner.nextLong
    }
  }

  implicit object FloatStringReader extends StringReader[Float] {
    def read(N: Int, scanner: Scanner): Seq[Float] = {
      for (i <- 0 until N) yield scanner.nextFloat
    }
  }

  implicit object DoubleStringReader extends StringReader[Double] {
    def read(N: Int, scanner: Scanner): Seq[Double] = {
      for (i <- 0 until N) yield scanner.nextDouble
    }
  }

}

trait EndianReader[@specialized(Byte, Char, Short, Int, Long, Float, Double) A] {
  def read(N: Int, is: InputStream, bo: ByteOrder): Seq[A]
}

object EndianReader {

  private def readBytes(is: InputStream, bo: ByteOrder, n: Int): ByteBuffer = {
    val byteChannel = Channels.newChannel(is)
    val byteBuffer = ByteBuffer.allocate(n).order(bo)
    byteChannel.read(byteBuffer)
    byteBuffer.flip()
    byteBuffer
  }

  implicit object ByteEndianReader extends EndianReader[Byte] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Byte] = {
      val n = N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.get
    }
  }

  implicit object CharEndianReader extends EndianReader[Char] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Char] = {
      val n = 2 * N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.getChar
    }
  }

  implicit object ShortEndianReader extends EndianReader[Short] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Short] = {
      val n = 2 * N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.getShort
    }
  }

  implicit object IntEndianReader extends EndianReader[Int] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Int] = {
      val n = 4 * N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.getInt
    }
  }

  implicit object LongEndianReader extends EndianReader[Long] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Long] = {
      val n = 8 * N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.getLong
    }
  }

  implicit object FloatEndianReader extends EndianReader[Float] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Float] = {
      val n = 4 * N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.getFloat
    }
  }

  implicit object DoubleEndianReader extends EndianReader[Double] {
    def read(N: Int, is: InputStream, bo: ByteOrder): Seq[Double] = {
      val n = 8 * N
      val byteBuffer = readBytes(is, bo, n)
      for (i <- 0 until N) yield byteBuffer.getDouble
    }
  }

}
