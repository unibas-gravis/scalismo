package scalismo.common

import scalismo.utils.ArrayUtils
import spire.math._

import scala.reflect.ClassTag

trait Scalar[@specialized(Short, Int, Long, Float, Double) S] extends Any {
  def fromByte(n: Byte): S
  def fromShort(n: Short): S
  def fromInt(n: Int): S
  def fromLong(n: Long): S
  def fromFloat(n: Float): S
  def fromDouble(n: Double): S

  def toByte(a: S): Byte
  def toShort(a: S): Short
  def toInt(a: S): Int
  def toLong(a: S): Long
  def toFloat(a: S): Float
  def toDouble(a: S): Double
}

abstract class DirectScalar[S: ClassTag] extends Scalar[S] {
  def createArray(data: Array[S]): DirectScalarArray[S] = new DirectScalarArray[S](data)
}

abstract class IndirectScalar[S, U: ClassTag] extends Scalar[S] {
  protected[scalismo] def convertArray[C](data: Array[C], f: C => S): IndirectScalarArray[S, U] = {
    createArray(ArrayUtils.fastMap[C, U](data, { c => toUnderlying(f(c)) }))
  }
  def createArray(data: Array[U]): IndirectScalarArray[S, U]
  protected def toUnderlying(s: S): U
  protected[common] def fromUnderlying(u: U): S
}

object Scalar {
  // Not exactly sure what this is good for, but spire seems to do it everywhere
  // for performance reasons. So we just do it as well.
  @inline final def apply[A](implicit ev: Scalar[A]): Scalar[A] = ev

  implicit final lazy val ByteIsScalar: DirectScalar[Byte] = Numeric.ByteIsNumeric
  implicit final lazy val ShortIsScalar: DirectScalar[Short] = Numeric.ShortIsNumeric
  implicit final lazy val IntIsScalar: DirectScalar[Int] = Numeric.IntIsNumeric
  implicit final lazy val LongIsScalar: DirectScalar[Long] = Numeric.LongIsNumeric
  implicit final lazy val FloatIsScalar: DirectScalar[Float] = Numeric.FloatIsNumeric
  implicit final lazy val DoubleIsScalar: DirectScalar[Double] = Numeric.DoubleIsNumeric

  implicit final lazy val UByteIsScalar: IndirectScalar[UByte, Byte] = new UByteIsScalar
  implicit final lazy val UShortIsScalar: IndirectScalar[UShort, Char] = new UShortIsScalar
  implicit final lazy val UIntIsScalar: IndirectScalar[UInt, Int] = new UIntIsScalar
  implicit final lazy val ULongIsScalar: IndirectScalar[ULong, Long] = new ULongIsScalar

  implicit class DirectScalarFromSpireNumeric[A: ClassTag](num: Numeric[A]) extends DirectScalar[A] {
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

    override def createArray(data: Array[Byte]): IndirectScalarArray[UByte, Byte] = IndirectScalarArray(data)(this)
    //override def createEmptyArray(length: Int): IndirectScalarArray[UByte, Byte] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    override def toUnderlying(u: UByte): Byte = u.toByte
    override def fromUnderlying(p: Byte): UByte = UByte(p)
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

    override def createArray(data: Array[Char]): IndirectScalarArray[UShort, Char] = IndirectScalarArray(data)(this)
    //override def createEmptyArray(length: Int): IndirectScalarArray[UShort, Char] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    override def toUnderlying(u: UShort): Char = u.toChar
    override def fromUnderlying(p: Char): UShort = UShort(p)
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

    override def createArray(data: Array[Int]): IndirectScalarArray[UInt, Int] = IndirectScalarArray(data)(this)
    //override def createEmptyArray(length: Int): IndirectScalarArray[UInt, Int] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    override def toUnderlying(u: UInt): Int = u.toInt
    override def fromUnderlying(p: Int): UInt = UInt(p)

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

    override def createArray(data: Array[Long]): IndirectScalarArray[ULong, Long] = IndirectScalarArray(data)(this)
    //override def createEmptyArray(length: Int): IndirectScalarArray[ULong, Long] = IndirectScalarArray(Array.ofDim(length), toPrimitive, fromPrimitive)(this)
    override def toUnderlying(u: ULong): Long = u.toLong
    override def fromUnderlying(p: Long): ULong = ULong(p)
  }
}

sealed trait ScalarArray[S] {
  def apply(index: Int): S
  def length: Int
  def size = length

  def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T]

  def iterator: Iterator[S] = ???
  def isDefinedAt(i: Int): Boolean = ???

  def toArray: Array[S] = ???
  def toSeq: Seq[S] = toArray.toSeq
  def deep: Object = ???
}

abstract case class AbstractScalarArray[S, U](protected val data: Array[U]) extends ScalarArray[S] {
  override def apply(index: Int): S = fromUnderlying(data(index))
  override def length: Int = data.length

  protected def fromUnderlying(u: U): S

  override def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T] = {
    val toScalar = implicitly[Scalar[T]]
    toScalar match {
      case s: DirectScalar[T] => s.createArray(data.map { u => f(fromUnderlying(u)) })
      case s: IndirectScalar[T, _] => s.convertArray[U](data, { u => f(fromUnderlying(u)) })
    }
  }

}

class DirectScalarArray[S: ClassTag](data: Array[S]) extends AbstractScalarArray[S, S](data) {
  override protected def fromUnderlying(u: S): S = u
}

class IndirectScalarArray[S, U](data: Array[U])(implicit scalar: IndirectScalar[S, U]) extends AbstractScalarArray[S, U](data) {
  override protected def fromUnderlying(u: U): S = scalar.fromUnderlying(u)
}

object IndirectScalarArray {
  def apply[A, B](array: Array[B])(implicit s: IndirectScalar[A, B]): IndirectScalarArray[A, B] = new IndirectScalarArray[A, B](array)(s)
}

object ScalarArray {
  def apply[T: Scalar: ClassTag](array: Array[T]): ScalarArray[T] = ???

  //  def apply[T: Scalar: ClassTag](array: Array[T]): ScalarArray[T] = {
  //    val toScalar = implicitly[Scalar[T]]
  //    toScalar match {
  //      case s: DirectScalar[T] => s.createArray(array)
  //      case s: IndirectScalar[T, _] =>
  //        val newArray = s.createEmptyArray(array.length)
  //        for (i <- 0 until newArray.length) { newArray(i) = array(i) }
  //        newArray
  //    }
  //  }

  // FIXME: THIS MUST GO AWAY
  implicit def arrayToScalarArray[A: Scalar: ClassTag](a: Array[A]): ScalarArray[A] = ??? //ScalarArray(a)
}
