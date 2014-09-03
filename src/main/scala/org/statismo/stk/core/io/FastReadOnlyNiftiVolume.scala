package org.statismo.stk.core.io

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.lang.{Short => JShort, Float => JFloat, Long => JLong, Double => JDouble}
import reflect.runtime.universe.{ TypeTag, typeOf }
import java.nio.channels.FileChannel

/**
 * This class implements a subset of the niftyjio.NiftyVolume functionality.
 * It is optimized to massively outperform the original class when reading .nii files,
 * however it only supports the functionality that is absolutely required for our use case.
 * This implementation only supports files < 2GB.
 *
 * @param raf the RandomAccessFile to be used
 */
class FastReadOnlyNiftiVolume private (private val raf: RandomAccessFile) {
  lazy val header: NiftiHeader = {
    val buf = ByteBuffer.allocate(348)
    raf.readFully(buf.array())

    if (buf.getInt(344) != 0x6e2b3100) {
      //0x6e2b3100 == "n+1\0"
      throw new IllegalArgumentException("This is not a supported Nifti file!")
    }

    new NiftiHeader(buf)
  }

  private lazy val transform = {
    val notransform = header.scl_slope == 0 || (header.scl_slope == 1.0f && header.scl_inter == 0.0f)
    !notransform
  }

  private lazy val doTransform = {
    val slope = header.scl_slope
    val inter = header.scl_inter
    def adjust(value: Double): Double = value * slope + inter
    adjust _
  }

  def dataArray: Array[Double] = this.synchronized {
    import NiftiHeader._
    val nx: Int = header.dim(1)
    val ny: Int = header.dim(2)
    var nz: Int = header.dim(3)
    var dim: Int = header.dim(4)
    if (header.dim(0) == 2) nz = 1
    if (dim == 0) dim = 1
    val array = new Array[Double](nx * ny * nz * dim)


    def loadBytes(unsigned: Boolean)(array: Array[Double]) = {
      val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length)
      for (index <- 0 until array.length) {
        val value = buf.get.toDouble
        val fixedValue = if (unsigned && value < 0) value + 256 else value
        array(index) = if(transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadShorts(unsigned: Boolean)(array: Array[Double]) = {
      val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 2)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) JShort.reverseBytes(buf.getShort) else buf.getShort).toDouble
        val fixedValue = if (unsigned && value < 0) Math.abs(value) + (1 << 15) else value
        array(index) = if(transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadInts(unsigned: Boolean)(array: Array[Double]) = {
      val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 4)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) Integer.reverseBytes(buf.getInt) else buf.getInt).toDouble
        val fixedValue = if (unsigned && value < 0) Math.abs(value) + (1 << 31) else value
        array(index) = if(transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadLongs(unsigned: Boolean)(array: Array[Double]) = {
      val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 8)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) JLong.reverseBytes(buf.getLong) else buf.getLong).toDouble
        val fixedValue = if (unsigned && value < 0) Math.abs(value) + (1 << 63) else value
        array(index) = if(transform) doTransform(fixedValue) else fixedValue
      }
    }

    def loadFloats(array: Array[Double]) = {
      val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 4)
      for (index <- 0 until array.length) {
        val value = (if (header.isLittleEndian) JFloat.intBitsToFloat(Integer.reverseBytes(buf.getInt)) else buf.getFloat).toDouble
        array(index) = if(transform) doTransform(value) else value
      }
    }

    def loadDoubles(array: Array[Double]) = {
      val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, header.vox_offset.toLong, array.length * 8)
      for (index <- 0 until array.length) {
        val value = if (header.isLittleEndian) JDouble.longBitsToDouble(JLong.reverseBytes(buf.getLong)) else buf.getDouble
        array(index) = if(transform) doTransform(value) else value
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

  class NiftiHeader(private val buf: ByteBuffer) {

    val isLittleEndian = {
      val s = buf.getShort(40)
      s < 1 || s > 7
    }

    class DirectArray[T: TypeTag](offset: Int, size: Int) {
      def apply(index: Int): T = {
        typeOf[T] match {
          case t if t <:< typeOf[Short] => shortAt(offset + 2 * index).asInstanceOf[T]
          case t if t <:< typeOf[Float] => floatAt(offset + 4 * index).asInstanceOf[T]
          case _ => throw new Throwable(s"Unsupported datatype ${typeOf[T]}")
        }
      }
    }

    private def shortAt(index: Int) = {
      val d = buf.getShort(index)
      if (isLittleEndian) JShort.reverseBytes(d) else d
    }

    private def floatAt(index: Int) = {
      if (isLittleEndian) JFloat.intBitsToFloat(Integer.reverseBytes(buf.getInt(index))) else buf.getFloat(index)
    }

    lazy val dim = new DirectArray[Short](40,8)
    lazy val datatype = shortAt(70)

    lazy val pixdim = new DirectArray[Float](76,8)

    lazy val sformArray: Array[Double] = {
      val doubles = new Array[Double](16)
      val floats = new DirectArray[Float](280, 12)
      for (i <- 0 until 12) {
        doubles(i) = floats(i)
      }
      doubles(15) = 1.0
      doubles
    }

    lazy val vox_offset = floatAt(108)
    lazy val scl_slope = floatAt(112)
    lazy val scl_inter = floatAt(116)
    lazy val sform_code = shortAt(254)
    
    lazy val srow_x = (0 until 4) .map(i => floatAt(280+i*4)).toArray
    lazy val srow_y = (0 until 4) .map(i => floatAt(296+i*4)).toArray
    lazy val srow_z =(0 until 4) .map(i => floatAt(312+i*4)).toArray
  }
}

object FastReadOnlyNiftiVolume {
  def read(filename: String): FastReadOnlyNiftiVolume = {
    new FastReadOnlyNiftiVolume(new RandomAccessFile(filename, "r"))
  }
}
