package scalismo.io

import java.io.{FileOutputStream, File, OutputStream}

/**
 * Type class for types that can be converted to an [[OutputStream]] for
 * generic write operations.
 *
 * The companion object includes implementations for [[OutputStream]] itself, and [[File]].
 *
 * @tparam A the type of objects that can be converted to an [[OutputStream]]
 */
trait ToOutputStream[A] {
  def toOutputStream(from: A): OutputStream
}

object ToOutputStream {

  implicit object OutputStreamToOutputStream extends ToOutputStream[OutputStream] {
    override def toOutputStream(from: OutputStream): OutputStream = from
  }

  implicit object FileToOutputStream extends ToOutputStream[File] {
    override def toOutputStream(from: File): OutputStream = new FileOutputStream(from)
  }
}
