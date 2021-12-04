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

import scalismo.common.Scalar.{FloatIsScalar, IntIsScalar}

import java.io.{File, RandomAccessFile}
import java.lang.{Double => JDouble, Float => JFloat, Long => JLong, Short => JShort}
import java.nio.channels.FileChannel
import java.nio.{ByteBuffer, MappedByteBuffer}
import scalismo.common.{Scalar, ScalarArray}
import scalismo.io.FastReadOnlyNiftiVolume.NiftiHeader
import spire.math.{UByte, UInt, UShort}

import scala.reflect.ClassTag
import scala.util.Try

/**
 * This class implements a subset of the niftijio.NiftyVolume functionality, maintaining
 * almost complete compatibility in terms of field and method names, except where alternative
 * implementations yield a significant performance gain. This affects the following two method
 * usages:
 *
 * 1. o.header.sform_to_mat44().flatten -> n.header.sformArray
 * 2. for (d <- 0 until dim; k <- 0 until nz; j <- 0 until ny; i <- 0 until nx) yield o.data(i)(j)(k)(d)
 * -> n.dataAsScalarArray
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
 * @param filename filename of a file in Nifti format (will be acccessed read only)
 */
class FastReadOnlyNiftiVolume private (private val filename: String) {
  lazy val header: NiftiHeader = {
    val buf = ByteBuffer.allocate(348)

    val file = new RandomAccessFile(filename, "r")
    file.readFully(buf.array())
    file.close()

    new NiftiHeader(buf)
  }

  val hasTransform = {
    // scl_slope == 0 -> no transformations
    // performance optimization: special case of slope=1, inter=0 is also detected and ignored (because a * 1 + 0 == a)
    val notransform = header.scl_slope == 0 || (header.scl_slope == 1.0f && header.scl_inter == 0.0f)

    !notransform
  }

  private val doTransform = {
    val slope = header.scl_slope
    val inter = header.scl_inter
    def adjust(value: Float): Float = value * slope + inter
    adjust _
  }

  /* the massive performance gain comes from two optimizations:
   * 1. mapping the file to memory using Java NIO, instead of using old-style Stream I/O
   * 2. (more or less) directly copying the flat data array(as present in the file), instead
   *    of performing back-and-forth transformations to/from a 4-dimensional array.
   */
  def dataAsScalarArray[S: Scalar: ClassTag]: ScalarArray[S] = this.synchronized {
    import NiftiHeader._

    val nx: Int = header.dim(1)
    val ny: Int = header.dim(2)
    var nz: Int = header.dim(3)
    var dim: Int = header.dim(4)
    if (header.dim(0) == 2) nz = 1
    if (dim == 0) dim = 1

    val arrayLength = nx * ny * nz * dim

    /**
     * This method loads the nifty data into a scalismo ScalarArray of the right type.
     * The output type O can be different from the input type U, due to the issue
     * with unsigned and signed types in Scala, which needs to be handled correctly.
     *
     * This method must only be called if the nifty file does not specify a transform
     * of the intensities. Otherwise loadArrayWithTransform,
     */
    def loadArray[U: ClassTag, O](sizeof: Int,
                                  load: MappedByteBuffer => U,
                                  toScalarArray: Array[U] => ScalarArray[O]): ScalarArray[O] = {
      assert(!hasTransform)
      val file = new RandomAccessFile(filename, "r")
      val channel = file.getChannel
      val mapped = channel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, arrayLength * sizeof)
      val data = Array.ofDim[U](arrayLength)

      mapped.load()

      var i = 0
      while (i < arrayLength) {
        data(i) = load(mapped)
        i += 1
      }

      channel.close()
      file.close()

      // ByteBuffer is only freed on garbage collection, so try to force that
      for (i <- 1 to 3) {
        System.gc()
      }

      toScalarArray(data)
    }

    /**
     * Loads the the data from the nifti volume, but transforms it first using the transform.
     * The result type is guaranteed to be of type float. The reason for converting to float is,
     * that if we kept the original type (e.g. unsigned short) but the intensities are shifted to
     * the negative, it would result in data loss. Converting everything to float might be a bit
     * wasteful in terms of memory, but avoids to many different type variants and also seems
     * to be what ITK (and hence 3D Slicer and itkSnap) is doing.
     */
    def loadArrayWithTransform[U: ClassTag](sizeof: Int,
                                            load: MappedByteBuffer => U,
                                            toFloat: U => Float): ScalarArray[Float] = {
      val file = new RandomAccessFile(filename, "r")
      val channel = file.getChannel
      val mapped = channel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, arrayLength * sizeof)
      val data = Array.ofDim[Float](arrayLength)

      mapped.load()

      var i = 0
      while (i < arrayLength) {
        val d = load(mapped)
        data(i) = doTransform(toFloat(d))
        i += 1
      }

      channel.close()
      file.close()

      // ByteBuffer is only freed on garbage collection, so try to force that
      for (i <- 1 to 3) {
        System.gc()
      }

      FloatIsScalar.createArray(data)
    }

    import Scalar._

    val loadShort = if (header.isLittleEndian) { (m: MappedByteBuffer) =>
      JShort.reverseBytes(m.getShort)
    } else { (m: MappedByteBuffer) =>
      m.getShort
    }
    val loadChar = { (m: MappedByteBuffer) =>
      loadShort(m).toChar
    }
    val loadInt = if (header.isLittleEndian) { (m: MappedByteBuffer) =>
      Integer.reverseBytes(m.getInt)
    } else { (m: MappedByteBuffer) =>
      m.getInt
    }
    val loadFloat = if (header.isLittleEndian) { (m: MappedByteBuffer) =>
      JFloat.intBitsToFloat(Integer.reverseBytes(m.getInt))
    } else { (m: MappedByteBuffer) =>
      m.getFloat
    }
    val loadDouble = if (header.isLittleEndian) { (m: MappedByteBuffer) =>
      JDouble.longBitsToDouble(JLong.reverseBytes(m.getLong))
    } else { (m: MappedByteBuffer) =>
      m.getDouble
    }

    val out = header.datatype match {
      case NIFTI_TYPE_INT8 =>
        if (hasTransform) {
          loadArrayWithTransform[Byte](1, _.get, _.toFloat)
        } else {
          loadArray[Byte, Byte](1, _.get, Scalar.ByteIsScalar.createArray)
        }

      case NIFTI_TYPE_UINT8 =>
        val toFloat = { x: Byte =>
          if (x >= 0) x.toFloat else x.toFloat + 256.0f
        }
        if (hasTransform) {
          loadArrayWithTransform[Byte](1, _.get, toFloat)
        } else {
          loadArray[Byte, UByte](1, _.get, UByteIsScalar.createArray)
        }
      case NIFTI_TYPE_INT16 =>
        if (hasTransform) {
          loadArrayWithTransform[Byte](1, _.get, _.toFloat)
        } else {
          loadArray[Short, Short](2, loadShort, ShortIsScalar.createArray)
        }

      case NIFTI_TYPE_UINT16 =>
        val toFloat = { x: Short =>
          if (x >= 0) x.toFloat else Math.abs(x.toFloat) + (1 << 15)
        }
        if (hasTransform) {
          loadArrayWithTransform[Char](2, loadChar, { x =>
            toFloat(x.toShort)
          })
        } else {
          loadArray[Char, UShort](2, loadChar, UShortIsScalar.createArray)
        }
      case NIFTI_TYPE_INT32 =>
        if (hasTransform) {
          loadArrayWithTransform[Int](4, _.get, _.toFloat)
        } else {
          loadArray[Int, Int](4, loadInt, IntIsScalar.createArray)
        }
      case NIFTI_TYPE_UINT32 =>
        val toFloat = { x: Int =>
          if (x >= 0) x.toFloat else Math.abs(x.toFloat) + (1 << 31)
        }
        if (hasTransform) {
          loadArrayWithTransform[Int](4, loadInt, toFloat)
        }
        loadArray[Int, UInt](4, loadInt, UIntIsScalar.createArray)

      case NIFTI_TYPE_FLOAT32 =>
        if (hasTransform) {
          loadArrayWithTransform[Float](4, loadFloat, _.toFloat)
        } else {
          loadArray[Float, Float](4, loadFloat, FloatIsScalar.createArray)
        }
      case NIFTI_TYPE_FLOAT64 =>
        if (hasTransform) {
          loadArrayWithTransform[Double](8, loadDouble, _.toFloat)
        } else {
          loadArray[Double, Double](8, loadDouble, DoubleIsScalar.createArray)
        }
      case _ => throw new UnsupportedOperationException(f"Unsupported Nifti data type ${header.datatype}")
    }

    out.asInstanceOf[ScalarArray[S]]
  }

}

object FastReadOnlyNiftiVolume {

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

    // check header size @ offset 0 (must be 348, whether in little- or big endian format is not specified)
    // and header magic @ offset 344 (the only supported value is "n+1")
    if ((buf.getInt(0) != 0x5c010000 && buf.getInt(0) != 348) || buf.getInt(344) != 0x6e2b3100) {
      throw new IllegalArgumentException("This is not a supported Nifti file!")
    }

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
    class DirectArray[T: Scalar](offset: Int, size: Int) {
      def apply(index: Int): T = {
        if (index < 0 || index >= size) throw new ArrayIndexOutOfBoundsException
        Scalar[T].scalarType match {
          case Scalar.ShortScalar => shortAt(offset + 2 * index).asInstanceOf[T]
          case Scalar.FloatScalar => floatAt(offset + 4 * index).asInstanceOf[T]
          case _                  => throw new Throwable(s"Unsupported datatype ${Scalar[T].scalarType}")
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

      val R: Array[Array[Double]] = Array.ofDim[Double](4, 4)

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
      } else {
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

  def read(filename: String): Try[FastReadOnlyNiftiVolume] = Try {
    new FastReadOnlyNiftiVolume(filename)
  }

  def getScalarType(file: File): Try[Short] = Try {
    val raf = new RandomAccessFile(file, "r")
    val buf = ByteBuffer.allocate(348)
    raf.readFully(buf.array())
    raf.close()
    new FastReadOnlyNiftiVolume.NiftiHeader(buf).datatype
  }
}
