/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.io

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
    lazy val qform_code = shortAt(252)
    lazy val sform_code = shortAt(254)
    lazy val quatern_bcd = new DirectArray[Float](256, 3)
    lazy val qoffset_xyz = new DirectArray[Float](268, 3)

    lazy val sformArray: Array[Double] = {
      val floats = new DirectArray[Float](280, 12)
      val doubles = new Array[Double](16)
      for (i <- 0 until 12) {
        doubles(i) = floats(i)
      }
      doubles(15) = 1.0
      doubles
    }

    lazy val qform_to_mat44: Array[Array[Double]] = {
      val qb: Double = quatern_bcd(0)
      val qc: Double = quatern_bcd(1)
      val qd: Double = quatern_bcd(2)
      val qx: Double = qoffset_xyz(0)
      val qy: Double = qoffset_xyz(1)
      val qz: Double = qoffset_xyz(2)
      val dx: Double = this.pixdim(1)
      val dy: Double = this.pixdim(2)
      val dz: Double = this.pixdim(3)
      val qfac: Double = this.pixdim(0)

      val R: Array[Array[Double]] = Array.ofDim[Double](4,4)

      /* last row is always [ 0 0 0 1 ] */
      R(3)(0) = 0.0
      R(3)(1) = 0.0
      R(3)(2) = 0.0
      R(3)(3) = 1.0
      var d: Double = qd
      var c: Double = qc
      var b: Double = qb
      var a: Double = 1.0 - (b * b + c * c + d * d)
      if (a < 1e-7) {
        a = 1.0 / Math.sqrt(b * b + c * c + d * d)
        b *= a
        c *= a
        d *= a
        a = 0.0
      }
      else {
        a = Math.sqrt(a)
      }
      val xd: Double = if (dx > 0.0) dx else 1.0
      val yd: Double = if (dy > 0.0) dy else 1.0
      var zd: Double = if (dz > 0.0) dz else 1.0
      if (qfac < 0.0) zd = -zd
      R(0)(0) = (a * a + b * b - c * c - d * d) * xd
      R(0)(1) = 2.0 * (b * c - a * d) * yd
      R(0)(2) = 2.0 * (b * d + a * c) * zd
      R(1)(0) = 2.0 * (b * c + a * d) * xd
      R(1)(1) = (a * a + c * c - b * b - d * d) * yd
      R(1)(2) = 2.0 * (c * d - a * b) * zd
      R(2)(0) = 2.0 * (b * d - a * c) * xd
      R(2)(1) = 2.0 * (c * d + a * b) * yd
      R(2)(2) = (a * a + d * d - c * c - b * b) * zd
      R(0)(3) = qx
      R(1)(3) = qy
      R(2)(3) = qz
      R
    }
  }
}

object FastReadOnlyNiftiVolume {
  def read(filename: String): Try[FastReadOnlyNiftiVolume] = Try {
    new FastReadOnlyNiftiVolume(new RandomAccessFile(filename, "r"))
  }
}
