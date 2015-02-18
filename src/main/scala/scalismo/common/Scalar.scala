package scalismo.common

import spire.math._

trait Scalar[@specialized(Short, Int, Long, Float, Double) A] extends Any {
  def fromByte(n: Byte): A
  def fromShort(n: Short): A
  def fromInt(n: Int): A
  def fromLong(n: Long): A
  def fromFloat(n: Float): A
  def fromDouble(n: Double): A

  def toByte(a: A): Byte
  def toShort(a: A): Short
  def toInt(a: A): Int
  def toLong(a: A): Long
  def toFloat(a: A): Float
  def toDouble(a: A): Double
}

object Scalar {
  // Not exactly sure what this is good for, but spire seems to do it everywhere
  // for performance reasons. So we just do it as well.
  @inline final def apply[A](implicit ev: Scalar[A]): Scalar[A] = ev

  implicit final lazy val ByteIsScalar: Scalar[Byte] = Numeric.ByteIsNumeric
  implicit final lazy val ShortIsScalar: Scalar[Short] = Numeric.ShortIsNumeric
  implicit final lazy val IntIsScalar: Scalar[Int] = Numeric.IntIsNumeric
  implicit final lazy val LongIsScalar: Scalar[Long] = Numeric.LongIsNumeric
  implicit final lazy val FloatIsScalar: Scalar[Float] = Numeric.FloatIsNumeric
  implicit final lazy val DoubleIsScalar: Scalar[Double] = Numeric.DoubleIsNumeric

  implicit final lazy val UByteIsScalar: Scalar[UByte] = new UByteIsScalar
  implicit final lazy val UShortIsScalar: Scalar[UShort] = new UShortIsScalar
  implicit final lazy val UIntIsScalar: Scalar[UInt] = new UIntIsScalar
  implicit final lazy val ULongIsScalar: Scalar[ULong] = new ULongIsScalar

  implicit private class ScalarFromSpireNumeric[A](num: Numeric[A]) extends Scalar[A] {
    override def toByte(a: A): Byte = num.toByte(a)
    override def toShort(a: A): Short = num.toShort(a)
    override def toInt(a: A): Int = num.toInt(a)
    override def toLong(a: A): Long = num.toLong(a)
    override def toFloat(a: A): Float = num.toFloat(a)
    override def toDouble(a: A): Double = num.toDouble(a)

    override def fromByte(n: Byte): A = num.fromByte(n)
    override def fromShort(n: Short): A = num.fromShort(n)
    override def fromInt(n: Int): A = num.fromInt(n)
    override def fromLong(n: Long): A = num.fromLong(n)
    override def fromFloat(n: Float): A = num.fromFloat(n)
    override def fromDouble(n: Double): A = num.fromDouble(n)

  }

  private class UByteIsScalar extends Scalar[UByte] {
    override def toByte(a: UByte): Byte = a.toByte
    override def toShort(a: UByte): Short = a.toShort
    override def toInt(a: UByte): Int = a.toInt
    override def toLong(a: UByte): Long = a.toLong
    override def toFloat(a: UByte): Float = a.toFloat
    override def toDouble(a: UByte): Double = a.toDouble

    override def fromByte(n: Byte): UByte = UByte(n.toByte)
    override def fromShort(n: Short): UByte = UByte(n.toByte)
    override def fromInt(n: Int): UByte = UByte(n.toByte)
    override def fromLong(n: Long): UByte = UByte(n.toByte)
    override def fromFloat(n: Float): UByte = UByte(n.toByte)
    override def fromDouble(n: Double): UByte = UByte(n.toByte)
  }

  private class UShortIsScalar extends Scalar[UShort] {
    override def toByte(a: UShort): Byte = a.toByte
    override def toShort(a: UShort): Short = a.toShort
    override def toInt(a: UShort): Int = a.toInt
    override def toLong(a: UShort): Long = a.toLong
    override def toFloat(a: UShort): Float = a.toFloat
    override def toDouble(a: UShort): Double = a.toDouble

    override def fromByte(n: Byte): UShort = UShort(n.toShort)
    override def fromShort(n: Short): UShort = UShort(n)
    override def fromInt(n: Int): UShort = UShort(n.toShort)
    override def fromLong(n: Long): UShort = UShort(n.toShort)
    override def fromFloat(n: Float): UShort = UShort(n.toShort)
    override def fromDouble(n: Double): UShort = UShort(n.toShort)
  }

  private class UIntIsScalar extends Scalar[UInt] {
    override def toByte(a: UInt): Byte = a.toByte
    override def toShort(a: UInt): Short = a.toShort
    override def toInt(a: UInt): Int = a.toInt
    override def toLong(a: UInt): Long = a.toLong
    override def toFloat(a: UInt): Float = a.toFloat
    override def toDouble(a: UInt): Double = a.toDouble

    override def fromByte(n: Byte): UInt = UInt(n.toInt)
    override def fromShort(n: Short): UInt = UInt(n.toInt)
    override def fromInt(n: Int): UInt = UInt(n)
    override def fromLong(n: Long): UInt = UInt(n.toInt)
    override def fromFloat(n: Float): UInt = UInt(n.toLong.toInt)
    override def fromDouble(n: Double): UInt = UInt(n.toLong.toInt)
  }

  class ULongIsScalar extends Scalar[ULong] {
    override def toByte(a: ULong): Byte = a.toByte
    override def toShort(a: ULong): Short = a.toShort
    override def toInt(a: ULong): Int = a.toInt
    override def toLong(a: ULong): Long = a.toLong
    /* toFloat() and toDouble() seem to be (currently) broken in spire.
     * Issue is filed here: https://github.com/non/spire/issues/387
     * For now, we take the detour via BigInt.
     */
    override def toFloat(a: ULong): Float = a.toBigInt.toFloat
    override def toDouble(a: ULong): Double = a.toBigInt.toDouble

    override def fromByte(n: Byte): ULong = ULong(n.toLong)
    override def fromShort(n: Short): ULong = ULong(n.toLong)
    override def fromInt(n: Int): ULong = ULong(n.toLong)
    override def fromLong(n: Long): ULong = ULong(n)

    /* Float.toLong() and Double.toLong() have the annoying habit of
     * capping their output to the range of Long. This is not an issue
     * for negative numbers (the result is likely to be wrong anyway,
     * as we don't expect negative numbers), but the ULong range is
     * actually twice as large as Long's when it comes to positive values.
     */
    override def fromFloat(n: Float): ULong = {
      val l = n.toLong
      l match {
        case Long.MaxValue => ULong(BigDecimal.exact(n).toLong)
        case _ => ULong(l)
      }
    }
    override def fromDouble(n: Double): ULong = {
      val l = n.toLong
      l match {
        case Long.MaxValue => ULong(BigDecimal.exact(n).toLong)
        case _ => ULong(l)
      }
    }
  }

}
