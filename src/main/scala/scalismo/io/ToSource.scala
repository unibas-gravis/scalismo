package scalismo.io

import java.io.{InputStream, File}

import scala.io.Source

/**
 * Type class for types that can be converted to a [[Source]] for
 * generic read operations.
 *
 * The companion object includes implementations for [[Source]] itself,
 * as well as for [[File]], [[InputStream]], [[String]], and byte arrays.
 *
 * @tparam A the type of objects that can be converted to a [[Source]]
 */

trait ToSource[A] {
  def toSource(input: A): Source
}

object ToSource {

  implicit object SourceToSource extends ToSource[Source] {
    override def toSource(input: Source): Source = input
  }

  implicit object FileToSource extends ToSource[File] {
    override def toSource(input: File): Source = Source.fromFile(input)
  }

  implicit object StringToSource extends ToSource[String] {
    override def toSource(input: String): Source = Source.fromString(input)
  }

  implicit object InputStreamToSource extends ToSource[InputStream] {
    override def toSource(input: InputStream): Source = Source.fromInputStream(input)
  }

  implicit object ByteArrayToSource extends ToSource[Array[Byte]] {
    override def toSource(input: Array[Byte]): Source = Source.fromBytes(input)
  }

}
