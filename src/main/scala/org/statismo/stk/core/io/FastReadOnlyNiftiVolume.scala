package org.statismo.stk.core.io

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.lang.{Short => JShort, Float => JFloat, Long => JLong, Double => JDouble}
import reflect.runtime.universe.{TypeTag, typeOf}
import java.nio.channels.FileChannel
import scala.util.Try

/**
 * This class implements a subset of the niftijio.NiftyVolume functionality, maintaining
 * almost complete compatibility in terms of field and method names, except where alternative
 * implementations yield a significant performance gain. This affects the following two method
 * usages:
 *
 * 1. o.header.sform_to_mat44().flatten -> n.header.sformArray
 * 2. for (d <- 0 until dim; k <- 0 until nz; j <- 0 until ny; i <- 0 until nx) yield o.data(i)(j)(k)(d)
 * -> n.dataArray
 *
 * This class is optimized to massively outperform the original implementation when reading .nii files,
 * however it only supports the functionality that is absolutely required for our use case
 * (meaning that, for instance, not all header fields are accessible, only the ones that we
 * actually use. If we need to evaluate more header fields in the future, this class will need
 * to be extended accordingly).
 *
 * This implementation only supports files < 2GB.
 *
 * For more information about the file format, see
 * http://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h ,
 * http://brainder.org/2012/09/23/the-nifti-file-format/ ,
 * and the niftijio.NiftiVolume and niftijio.NiftiHeader classes
 *
 * @param file the RandomAccessFile in Nifti format (read only)
 */
class FastReadOnlyNiftiVolume private(private val file: RandomAccessFile) {
  lazy val header: NiftiHeader = {
    val buf = ByteBuffer.allocate(348)
    file.readFully(buf.array())

    // check header size @ offset 0 (must be 348, whether in little- or big endian format is not specified)
    // and header magic @ offset 344 (the only supported value is "n+1")
    if ((buf.getInt(0) != 0x5c010000 && buf.getInt(0) != 348) || buf.getInt(344) != 0x6e2b3100) {
      throw new IllegalArgumentException("This is not a supported Nifti file!")
    }

    new NiftiHeader(buf)
  }

  private lazy val transform = {
    // scl_slope == 0 -> no transformation
    // performance optimization: special case of slope=1, inter=0 is also detected and ignored (because a * 1 + 0 == a)
    val notransform = header.scl_slope == 0 || (header.scl_slope == 1.0f && header.scl_inter == 0.0f)
    !notransform
  }

  private lazy val doTransform = {
    val slope = header.scl_slope
    val inter = header.scl_inter
    def adjust(value: Double): Double = value * slope + inter
    adjust _
  }

  /* the massive performance gain comes from two optimizations:
   * 1. mapping the file to memory using Java NIO, instead of using old-style Stream I/O
   * 2. (more or less) directly copying the flat data array(as present in the file), instead
   *    of performing back-and-forth transformations to/from a 4-dimensional array.
   */
  def dataArray: Array[Double] = this.synchronized {
    import NiftiHeader._
    val nx: Int = header.dim(1)
    val ny: Int = header.dim(2)
    var nz: Int = header.dim(3)
    var dim: Int = header.dim(4)
    if (header.dim(0) == 2) nz = 1
    if (dim == 0) dim = 1
    val array = new Array[Double](nx * ny * nz * dim)

    // all these methods do the same thing, they just decode different data representations.
    def loadBytes(unsigned: Boolean)(array: Array[Double]) = {
      val buf = file.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length)
      for (index <- 0 until array.length) {
        val value = buf.get.toDouble
        val fixedValue = if (unsigned && value < 0) value + 256 else value
        array(index) = if (transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadShorts(unsigned: Boolean)(array: Array[Double]) = {
      val buf = file.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 2)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) JShort.reverseBytes(buf.getShort) else buf.getShort).toDouble
        val fixedValue = if (unsigned && value < 0) Math.abs(value) + (1 << 15) else value
        array(index) = if (transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadInts(unsigned: Boolean)(array: Array[Double]) = {
      val buf = file.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 4)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) Integer.reverseBytes(buf.getInt) else buf.getInt).toDouble
        val fixedValue = if (unsigned && value < 0) Math.abs(value) + (1 << 31) else value
        array(index) = if (transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadLongs(unsigned: Boolean)(array: Array[Double]) = {
      val buf = file.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 8)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) JLong.reverseBytes(buf.getLong) else buf.getLong).toDouble
        val fixedValue = if (unsigned && value < 0) Math.abs(value) + (1 << 63) else value
        array(index) = if (transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadFloats(array: Array[Double]) = {
      val buf = file.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 4)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) JFloat.intBitsToFloat(Integer.reverseBytes(buf.getInt)) else buf.getFloat).toDouble
        array(index) = if (transform) doTransform(value) else value
      }
    }

    def loadDoubles(array: Array[Double]) = {
      val buf = file.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 8)
      for (index <- 0 until array.length) {
        val value = if (header.isLittleEndian) JDouble.longBitsToDouble(JLong.reverseBytes(buf.getLong)) else buf.getDouble
        array(index) = if (transform) doTransform(value) else value
      }
    }

    val load = header.datatype match {
      case NIFTI_TYPE_INT8 => loadBytes(unsigned = false)(_)
      case NIFTI_TYPE_UINT8 => loadBytes(unsigned = true)(_)

      case NIFTI_TYPE_INT16 => loadShorts(unsigned = false)(_)
      case NIFTI_TYPE_UINT16 => loadShorts(unsigned = true)(_)

      case NIFTI_TYPE_INT32 => loadInts(unsigned = false)(_)
      case NIFTI_TYPE_UINT32 => loadInts(unsigned = true)(_)

      case NIFTI_TYPE_INT64 => loadLongs(unsigned = false)(_)
      case NIFTI_TYPE_UINT64 => loadLongs(unsigned = true)(_)

      case NIFTI_TYPE_FLOAT32 => loadFloats _
      case NIFTI_TYPE_FLOAT64 => loadDoubles _

      case _ => throw new UnsupportedOperationException(f"Unsupported Nifti data type ${header.datatype}")
    }

    load(array)

    array
  }

  object NiftiHeader {
    final val NIFTI_TYPE_UINT8: Short = 2
    final val NIFTI_TYPE_INT16: Short = 4
    final val NIFTI_TYPE_INT32: Short = 8
    final val NIFTI_TYPE_FLOAT32: Short = 16
    final val NIFTI_TYPE_FLOAT64: Short = 64
    final val NIFTI_TYPE_INT8: Short = 256
    final val NIFTI_TYPE_UINT16: Short = 512
    final val NIFTI_TYPE_UINT32: Short = 768
    final val NIFTI_TYPE_INT64: Short = 1024
    final val NIFTI_TYPE_UINT64: Short = 1280
  }

  /* For performance reasons, the entire 348-byte header is read into a ByteBuffer,
   * thus allowing to directly retrieve values at a given offset. For the header
   * specification, including offsets, see
   * http://brainder.org/2012/09/23/the-nifti-file-format/
   */
  class NiftiHeader(private val buf: ByteBuffer) {

    val isLittleEndian = {
      // according to the documentation, dim(0) (@offset 40) should be between 1 and 7.
      // if it's not, then the data is in little-endian format (says the documentation).
      val s = buf.getShort(40)
      s < 1 || s > 7
    }

    private def shortAt(offset: Int) = {
      val d = buf.getShort(offset)
      if (isLittleEndian) JShort.reverseBytes(d) else d
    }

    private def floatAt(offset: Int) = {
      if (isLittleEndian) JFloat.intBitsToFloat(Integer.reverseBytes(buf.getInt(offset))) else buf.getFloat(offset)
    }

    /* This class mimics a (read-only) array, but gets its data directly from the
     * underlying ByteBuffer. For instance, the below definition
     *   lazy val dim = new DirectArray[Short](40,8)
     * means: at offset 40, there are 8 consecutive Short values, which can be retrieved
     * as dim(0) .. dim(7).
     */
    class DirectArray[T: TypeTag](offset: Int, size: Int) {
      def apply(index: Int): T = {
        if (index < 0 || index >= size) throw new ArrayIndexOutOfBoundsException
        typeOf[T] match {
          case t if t <:< typeOf[Short] => shortAt(offset + 2 * index).asInstanceOf[T]
          case t if t <:< typeOf[Float] => floatAt(offset + 4 * index).asInstanceOf[T]
          case _ => throw new Throwable(s"Unsupported datatype ${typeOf[T]}")
        }
      }
    }

    lazy val dim = new DirectArray[Short](40, 8)
    lazy val datatype = shortAt(70)
    lazy val pixdim = new DirectArray[Float](76, 8)

    lazy val vox_offset = floatAt(108)
    lazy val scl_slope = floatAt(112)
    lazy val scl_inter = floatAt(116)
    lazy val sform_code = shortAt(254)

    lazy val sformArray: Array[Double] = {
      val floats = new DirectArray[Float](280, 12)
      val doubles = new Array[Double](16)
      for (i <- 0 until 12) {
        doubles(i) = floats(i)
      }
      doubles(15) = 1.0
      doubles
    }
  }
}

object FastReadOnlyNiftiVolume {
  def read(filename: String): Try[FastReadOnlyNiftiVolume] = Try {
    new FastReadOnlyNiftiVolume(new RandomAccessFile(filename, "r"))
  }
}
