package scalismo.common

import scalismo.utils.ArrayUtils
import spire.math._

import scala.reflect.ClassTag

// TODO: see the comment on ValueClassScalar below.
trait Scalar[@specialized(Byte, Short, Int, Long, Float, Double) S] extends Any {
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

abstract class PrimitiveScalar[S <: AnyVal: ClassTag] extends Scalar[S] {
  def createArray(data: Array[S]): PrimitiveScalarArray[S] = new PrimitiveScalarArray[S](data)
}

/* TODO:    we need to discuss if we want to (at least theoretically) allow for
 * TODO:    "non-value-class" scalars. It's possible in theory, but may be a huge performance
 * TODO:    problem if we allow it.
 */
abstract class ValueClassScalar[S <: AnyVal, U <: AnyVal: ClassTag] extends Scalar[S] {

  protected[scalismo] def convertArray[C](data: Array[C], f: C => S): ValueClassScalarArray[S, U] = {
    createArray(ArrayUtils.fastMap[C, U](data, { c => toUnderlying(f(c)) }))
  }
  def createArray(data: Array[U]): ValueClassScalarArray[S, U]

  protected[common] def toUnderlying(s: S): U
  protected[common] def fromUnderlying(u: U): S
}

object Scalar {
  // Not exactly sure what this is good for, but spire seems to do it everywhere
  // for performance reasons. So we just do it as well.
  @inline final def apply[A <: AnyVal](implicit ev: Scalar[A]): Scalar[A] = ev

  implicit final lazy val ByteIsScalar: PrimitiveScalar[Byte] = Numeric.ByteIsNumeric
  implicit final lazy val ShortIsScalar: PrimitiveScalar[Short] = Numeric.ShortIsNumeric
  implicit final lazy val IntIsScalar: PrimitiveScalar[Int] = Numeric.IntIsNumeric
  implicit final lazy val LongIsScalar: PrimitiveScalar[Long] = Numeric.LongIsNumeric
  implicit final lazy val FloatIsScalar: PrimitiveScalar[Float] = Numeric.FloatIsNumeric
  implicit final lazy val DoubleIsScalar: PrimitiveScalar[Double] = Numeric.DoubleIsNumeric

  implicit final lazy val UByteIsScalar: ValueClassScalar[UByte, Byte] = new UByteIsScalar
  implicit final lazy val UShortIsScalar: ValueClassScalar[UShort, Char] = new UShortIsScalar
  implicit final lazy val UIntIsScalar: ValueClassScalar[UInt, Int] = new UIntIsScalar
  implicit final lazy val ULongIsScalar: ValueClassScalar[ULong, Long] = new ULongIsScalar

  implicit class PrimitiveScalarFromSpireNumeric[A <: AnyVal: ClassTag](num: Numeric[A]) extends PrimitiveScalar[A] {
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

  class UByteIsScalar extends ValueClassScalar[UByte, Byte] {
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

    override def createArray(data: Array[Byte]): ValueClassScalarArray[UByte, Byte] = ValueClassScalarArray(data)(this)
    override def toUnderlying(u: UByte): Byte = u.toByte
    override def fromUnderlying(p: Byte): UByte = UByte(p)
  }

  class UShortIsScalar extends ValueClassScalar[UShort, Char] {
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

    override def createArray(data: Array[Char]): ValueClassScalarArray[UShort, Char] = ValueClassScalarArray(data)(this)
    override def toUnderlying(u: UShort): Char = u.toChar
    override def fromUnderlying(p: Char): UShort = UShort(p)
  }

  class UIntIsScalar extends ValueClassScalar[UInt, Int] {
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

    override def createArray(data: Array[Int]): ValueClassScalarArray[UInt, Int] = ValueClassScalarArray(data)(this)
    override def toUnderlying(u: UInt): Int = u.toInt
    override def fromUnderlying(p: Int): UInt = UInt(p)

  }

  class ULongIsScalar extends ValueClassScalar[ULong, Long] {
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

    override def createArray(data: Array[Long]): ValueClassScalarArray[ULong, Long] = ValueClassScalarArray(data)(this)
    override def toUnderlying(u: ULong): Long = u.toLong
    override def fromUnderlying(p: Long): ULong = ULong(p)
  }
}

sealed trait ScalarArray[S] {
  def apply(index: Int): S
  def length: Int
  final lazy val size = length
  final def isDefinedAt(index: Int): Boolean = index < size && index >= 0

  def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T]

  //  @deprecated("discouraged - you may be instantiating value classes if constructing collections from the iterator", "always")
  def iterator: Iterator[S]
}

abstract case class AbstractScalarArray[S, U](protected val rawData: Array[U]) extends ScalarArray[S] {
  override final def length: Int = rawData.length

  protected def fromUnderlying(u: U): S

  override def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T] = {
    val toScalar = implicitly[Scalar[T]]
    toScalar match {
      case s: PrimitiveScalar[T] => s.createArray(rawData.map { u => f(fromUnderlying(u)) })
      case s: ValueClassScalar[T, _] => s.convertArray[U](rawData, { u => f(fromUnderlying(u)) })
    }
  }

}

final class PrimitiveScalarArray[S <: AnyVal: ClassTag](rawData: Array[S]) extends AbstractScalarArray[S, S](rawData) {
  // FIXME: need to implement hashCode, equals etc.?

  override protected def fromUnderlying(u: S): S = u
  override def apply(index: Int): S = rawData(index)

  //  @deprecated("discouraged - you may be instantiating value classes if constructing collections from the iterator", "always")
  override def iterator: Iterator[S] = rawData.iterator
}

final class ValueClassScalarArray[S <: AnyVal, U <: AnyVal](rawData: Array[U])(implicit scalar: ValueClassScalar[S, U]) extends AbstractScalarArray[S, U](rawData) {
  // FIXME: need to implement hashCode, equals etc.?

  override protected def fromUnderlying(u: U): S = scalar.fromUnderlying(u)
  override def apply(index: Int): S = fromUnderlying(rawData(index))

  //  @deprecated("discouraged - you may be instantiating value classes if constructing collections from the iterator", "always")
  override def iterator: Iterator[S] = rawData.iterator.map(scalar.fromUnderlying)
}

object ValueClassScalarArray {
  def apply[S <: AnyVal, U <: AnyVal](array: Array[U])(implicit s: ValueClassScalar[S, U]): ValueClassScalarArray[S, U] = new ValueClassScalarArray[S, U](array)(s)
}

object ScalarArray {

  @deprecated("THIS METHOD *** WILL *** BE REMOVED BECAUSE IT FORCES INSTANTIATION OF VALUE CLASSES", "always")
  def apply[T: Scalar: ClassTag](array: Array[T]): ScalarArray[T] = {
    val scalar = implicitly[Scalar[T]]
    scalar match {
      case p: PrimitiveScalar[T] => p.createArray(array)
      case v: ValueClassScalar[T, _] => v.convertArray[T](array, { t => t })
    }
  }

  object implicits {
    import Scalar._
    import scala.language.implicitConversions
    implicit def byteArray(data: Array[Byte]): ScalarArray[Byte] = ByteIsScalar.createArray(data)
    implicit def shortArray(data: Array[Short]): ScalarArray[Short] = ShortIsScalar.createArray(data)
    implicit def intArray(data: Array[Int]): ScalarArray[Int] = IntIsScalar.createArray(data)
    implicit def longArray(data: Array[Long]): ScalarArray[Long] = LongIsScalar.createArray(data)
    implicit def floatArray(data: Array[Float]): ScalarArray[Float] = FloatIsScalar.createArray(data)
    implicit def doubleArray(data: Array[Double]): ScalarArray[Double] = DoubleIsScalar.createArray(data)
  }
}
