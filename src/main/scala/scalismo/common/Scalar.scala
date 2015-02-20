package scalismo.common

import spire.math._

import scala.reflect.ClassTag

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

trait DirectScalar[A] extends Scalar[A] {
  def createArray(data: Array[A]): DirectScalarArray[A] = new DirectScalarArray[A](data)(this)
}

trait IndirectScalar[A, B] extends Scalar[A] {
  def createArray(data: Array[B]): IndirectScalarArray[A, B]
  def createEmptyArray(length: Int): IndirectScalarArray[A, B]
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

  implicit class DirectScalarFromSpireNumeric[A](num: Numeric[A]) extends DirectScalar[A] {
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

  class UByteIsScalar extends IndirectScalar[UByte, Byte] {
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

    override def createArray(data: Array[Byte]): IndirectScalarArray[UByte, Byte] = IndirectScalarArray(data, toPrimitive, fromPrimitive)(this)
    override def createEmptyArray(length: Int): IndirectScalarArray[UByte, Byte] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    private def toPrimitive(u: UByte): Byte = u.toByte
    private def fromPrimitive(p: Byte): UByte = UByte(p)
  }

  class UShortIsScalar extends IndirectScalar[UShort, Char] {
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

    override def createArray(data: Array[Char]): IndirectScalarArray[UShort, Char] = IndirectScalarArray(data, toPrimitive, fromPrimitive)(this)
    override def createEmptyArray(length: Int): IndirectScalarArray[UShort, Char] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    private def toPrimitive(u: UShort): Char = u.toChar
    private def fromPrimitive(p: Char): UShort = UShort(p)
  }

  class UIntIsScalar extends IndirectScalar[UInt, Int] {
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

    override def createArray(data: Array[Int]): IndirectScalarArray[UInt, Int] = IndirectScalarArray(data, toPrimitive, fromPrimitive)(this)
    override def createEmptyArray(length: Int): IndirectScalarArray[UInt, Int] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    private def toPrimitive(u: UInt): Int = u.toInt
    private def fromPrimitive(p: Int): UInt = UInt(p)

  }

  class ULongIsScalar extends IndirectScalar[ULong, Long] {
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
        case Long.MaxValue => ULong(BigDecimal(n.toDouble).toLong)
        case _ => ULong(l)
      }
    }
    override def fromDouble(n: Double): ULong = {
      val l = n.toLong
      l match {
        case Long.MaxValue => ULong(BigDecimal(n).toLong)
        case _ => ULong(l)
      }
    }

    override def createArray(data: Array[Long]): IndirectScalarArray[ULong, Long] = IndirectScalarArray(data, toPrimitive, fromPrimitive)(this)
    override def createEmptyArray(length: Int): IndirectScalarArray[ULong, Long] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    private def toPrimitive(u: ULong): Long = u.toLong
    private def fromPrimitive(p: Long): ULong = ULong(p)
  }
}

sealed trait ScalarArray[S] {
  def apply(index: Int): S
  def update(index: Int, value: S)
  def length: Int

  def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T]
}

class DirectScalarArray[A: Scalar](private val array: Array[A]) extends ScalarArray[A] {
  override def apply(index: Int): A = array(index)
  override def update(index: Int, value: A): Unit = array(index) = value
  override def length = array.length

  override def map[T: Scalar: ClassTag](f: A => T): ScalarArray[T] = {
    val toScalar = implicitly[Scalar[T]]
    toScalar match {
      case s: DirectScalar[T] => s.createArray(array.map { b => f(b) })
      case s: IndirectScalar[T, _] =>
        val newArray = s.createEmptyArray(array.length)
        for (i <- 0 until newArray.length) { newArray(i) = f(array(i)) }
        newArray
    }
  }

}

class IndirectScalarArray[A, B](private val array: Array[B], toPrimitive: A => B, fromPrimitive: B => A)(implicit s: IndirectScalar[A, B]) extends ScalarArray[A] {
  override def apply(index: Int): A = fromPrimitive(array(index))
  override def update(index: Int, value: A): Unit = array(index) = toPrimitive(value)
  override def length = array.length

  override def map[T: Scalar: ClassTag](f: A => T): ScalarArray[T] = {
    val toScalar = implicitly[Scalar[T]]
    toScalar match {
      case s: DirectScalar[T] => s.createArray(array.map { b => f(fromPrimitive(b)) })
      case s: IndirectScalar[T, _] =>
        val newArray = s.createEmptyArray(array.length)
        for (i <- 0 until newArray.length) { newArray(i) = f(fromPrimitive(array(i))) }
        newArray
    }
  }
}

object IndirectScalarArray {
  def apply[A, B](array: Array[B], toPrimitive: A => B, fromPrimitive: B => A)(implicit s: IndirectScalar[A, B]): IndirectScalarArray[A, B] = new IndirectScalarArray[A, B](array, toPrimitive, fromPrimitive)(s)
}
