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

import java.io.{OutputStream, OutputStreamWriter}
import java.nio.{ByteBuffer, ByteOrder}

/**
 * The ListWriter writes a sequence of numbers of elements of type `A` with the number of elements as prefix.
 *
 * ASCII/String example for A:Double write(Seq(0.0,1.0,2.0)) => "3 0.0 1.0 2.0"
 *
 * @tparam A
 *   ElementType of the sequence to be written.
 */
class ListWriter[@specialized(Byte, Char, Short, Int, Long, Float, Double) A: StringWriter: EndianWriter] {
  val seqWriter = new SequenceWriter[A]

  def write(seq: Seq[A], osw: OutputStreamWriter): Unit = {
    osw.write("%d ".format(seq.length))
    seqWriter.write(seq, osw)
  }

  def write(seq: Seq[A], os: OutputStream, bo: ByteOrder): Unit = {
    val buffer: ByteBuffer = ByteBuffer.allocate(1)
    buffer.order(bo)
    buffer.put(seq.size.toByte)
    os.write(buffer.array())
    seqWriter.write(seq, os, bo)
  }
}

/**
 * The SequenceWriter writes a sequence of numbers of type `A`.
 *
 * ASCII/String example for A:Double write(Seq(0.0,1.0,2.0)) => "0.0 1.0 2.0"
 *
 * @tparam A
 *   ElementType of the sequence to be written.
 */
class SequenceWriter[@specialized(Byte, Char, Short, Int, Long, Float, Double) A: StringWriter: EndianWriter] {

  def write(seq: Iterable[A], osw: OutputStreamWriter): Unit = {
    implicitly[StringWriter[A]].write(seq, osw)
  }

  def write(seq: Iterable[A], os: OutputStream, bo: ByteOrder): Unit = {
    implicitly[EndianWriter[A]].write(seq, os, bo)
  }
}

trait StringWriter[@specialized(Byte, Char, Short, Int, Long, Float, Double) A] {
  def write(seq: Iterable[A], osw: OutputStreamWriter): Unit
}

object StringWriter {
  implicit object ByteStringWriter extends StringWriter[Byte] {
    def write(a: Iterable[Byte], osw: OutputStreamWriter): Unit = {
      osw.write(a.map(b => (b + 256) % 256).mkString(" "))
    }
  }

  implicit object CharStringWriter extends StringWriter[Char] {
    def write(a: Iterable[Char], osw: OutputStreamWriter): Unit = {
      osw.write(a.map(_.toInt).mkString(" "))
    }
  }

  implicit object ShortStringWriter extends StringWriter[Short] {
    def write(a: Iterable[Short], osw: OutputStreamWriter): Unit = {
      osw.write(a.mkString(" "))
    }
  }

  implicit object IntStringWriter extends StringWriter[Int] {
    def write(a: Iterable[Int], osw: OutputStreamWriter): Unit = {
      osw.write(a.mkString(" "))
    }
  }

  implicit object LongStringWriter extends StringWriter[Long] {
    def write(a: Iterable[Long], osw: OutputStreamWriter): Unit = {
      osw.write(a.mkString(" "))
    }
  }

  implicit object FloatStringWriter extends StringWriter[Float] {
    def write(a: Iterable[Float], osw: OutputStreamWriter): Unit = {
      osw.write(a.map(f => "%.8g".formatLocal(java.util.Locale.US, f)).mkString(" "))
    }
  }

  implicit object DoubleStringWriter extends StringWriter[Double] {
    def write(a: Iterable[Double], osw: OutputStreamWriter): Unit = {
      osw.write(a.map(d => "%.17g".formatLocal(java.util.Locale.US, d)).mkString(" "))
    }
  }

}

trait EndianWriter[@specialized(Byte, Char, Short, Int, Long, Float, Double) A] {
  def write(seq: Iterable[A], os: OutputStream, bo: ByteOrder): Unit
}

object EndianWriter {

  implicit object ByteEndianWriter extends EndianWriter[Byte] {
    def write(a: Iterable[Byte], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(a.size)
      buffer.order(bo)
      a.foreach(buffer.put)
      os.write(buffer.array())
    }
  }

  implicit object CharEndianWriter extends EndianWriter[Char] {
    def write(a: Iterable[Char], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(2 * a.size)
      buffer.order(bo)
      a.foreach(buffer.putChar)
      os.write(buffer.array())
      os.flush()
    }
  }

  implicit object ShortEndianWriter extends EndianWriter[Short] {
    def write(a: Iterable[Short], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(2 * a.size)
      buffer.order(bo)
      a.foreach(buffer.putShort)
      os.write(buffer.array())
      os.flush()
    }
  }

  implicit object IntEndianWriter extends EndianWriter[Int] {
    def write(a: Iterable[Int], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(4 * a.size)
      buffer.order(bo)
      a.foreach(buffer.putInt)
      os.write(buffer.array())
      os.flush()
    }
  }

  implicit object LongEndianWriter extends EndianWriter[Long] {
    def write(a: Iterable[Long], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(8 * a.size)
      buffer.order(bo)
      a.foreach(buffer.putLong)
      os.write(buffer.array())
      os.flush()
    }
  }

  implicit object FloatEndianWriter extends EndianWriter[Float] {
    def write(a: Iterable[Float], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(4 * a.size)
      buffer.order(bo)
      a.foreach(buffer.putFloat)
      os.write(buffer.array())
      os.flush()
    }
  }

  implicit object DoubleEndianWriter extends EndianWriter[Double] {
    def write(a: Iterable[Double], os: OutputStream, bo: ByteOrder): Unit = {
      val buffer: ByteBuffer = ByteBuffer.allocate(8 * a.size)
      buffer.order(bo)
      a.foreach(buffer.putDouble)
      os.write(buffer.array())
      os.flush()
    }
  }

}
