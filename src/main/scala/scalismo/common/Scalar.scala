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
package scalismo.common

import scalismo.utils.ArrayUtils
import spire.math._

import scala.reflect.ClassTag

/**
 * Trait signifying that the data is scalar (i.e., numeric).
 *
 * Note that while unsigned integral types (UByte, UShort, UInt, ULong) are supported,
 * some operations may be significantly slower than when using the built-in primitive
 * (signed) data types. In other words, it may be worthwile to directly map the
 * data to a signed type after reading, then working with the signed data.
 *
 * @tparam S the type of the actual scalar data.
 */
trait Scalar[@specialized(Byte, Short, Int, Float, Double) S] extends Any {
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

/**
 * Trait signifying that the data is scalar, and of a primitive JVM type.
 *
 * @tparam S the (primitive) type of the actual scalar data.
 */
abstract class PrimitiveScalar[S <: AnyVal: ClassTag] extends Scalar[S] {
  def createArray(data: Array[S]): PrimitiveScalarArray[S] = new PrimitiveScalarArray[S](data)
}

/**
 * Trait signifying that the data is scalar, and is using a value class to "wrap" the values of
 * an underlying primitive type.
 * @tparam S the exposed (value class) type of the scalar data.
 * @tparam U the underlying (primitive) type of the actual stored values.
 * @see <a href="http://docs.scala-lang.org/overviews/core/value-classes.html">Value Classes and Universal Traits</a>
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
  implicit final lazy val FloatIsScalar: PrimitiveScalar[Float] = Numeric.FloatIsNumeric
  implicit final lazy val DoubleIsScalar: PrimitiveScalar[Double] = Numeric.DoubleIsNumeric

  implicit final lazy val UByteIsScalar: ValueClassScalar[UByte, Byte] = new UByteIsScalar
  implicit final lazy val UShortIsScalar: ValueClassScalar[UShort, Char] = new UShortIsScalar
  implicit final lazy val UIntIsScalar: ValueClassScalar[UInt, Int] = new UIntIsScalar

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

}

/**
 * Class representing an array of scalar data. Only a subset of the array and generic collections operations is supported,
 * and the data should be treated as immutable. For instance, data values can be accessed by index, but not updated.
 * @tparam S the type of the contained data.
 */

sealed trait ScalarArray[S] extends IndexedSeq[S] {
  /**
   * Returns the <code>index</code>th element of the array
   * @param index the index of the value to return
   * @return the value at index <code>index</code>
   */
  def apply(index: Int): S

  /**
   * Returns the length of the data array.
   * @return the length of the data array
   */
  def length: Int

  /**
   * Returns the length of the data array. This is an alias for [[ScalarArray#length]]
   */
  override final lazy val size = length

  /**
   * Determines if <code>index</code> lies within the bounds of the array
   * @param index the index in the array for which to check if it lies within the array bounds
   * @return <code>true</code> if <code>index</code> lies within the array bounds, <code>false</code> otherwise.
   */
  override final def isDefinedAt(index: Int): Boolean = index < size && index >= 0

  /**
   * Maps this [[ScalarArray]] to another [[ScalarArray]] using the given mapping function
   * @param f the mapping function to use
   * @tparam T the type of the values of the resulting [[ScalarArray]]
   * @return a new [[ScalarArray]] whose values correspond to the values of this instance, mapped by the function <code>f</code>
   */
  def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T]

  /**
   * Returns an iterator over the array's values.
   * @return an iterator over the array's values.
   */
  def iterator: Iterator[S]
}

/**
 * Basic implementation of [[ScalarArray]], common to both primitive and value-class scalar arrays.
 * @param rawData the actual raw data contained in the array
 * @tparam S the type of the contained data.
 * @tparam U the type of the underlying contained raw data
 */
abstract case class AbstractScalarArray[S, U](protected[scalismo] val rawData: Array[U]) extends ScalarArray[S] {
  /**
   * Returns the length of the data array.
   * @return the length of the data array
   */
  override final def length: Int = rawData.length

  /**
   * Convert one datum from the underlying type to the [[ScalarArray]]'s type
   * @param u a value of the underlying type
   * @return the corresponding value of the array type
   */
  protected def fromUnderlying(u: U): S

  /**
   * Maps this [[ScalarArray]] to another [[ScalarArray]] using the given mapping function
   * @param f the mapping function to use
   * @tparam T the type of the values of the resulting [[ScalarArray]]
   * @return a new [[ScalarArray]] whose values correspond to the values of this instance, mapped by the function <code>f</code>
   */
  override def map[T: Scalar: ClassTag](f: S => T): ScalarArray[T] = {
    val toScalar = implicitly[Scalar[T]]
    toScalar match {
      case s: PrimitiveScalar[T] => s.createArray(rawData.map { u => f(fromUnderlying(u)) })
      case s: ValueClassScalar[T, _] => s.convertArray[U](rawData, { u => f(fromUnderlying(u)) })
    }
  }

}

/**
 * A [[ScalarArray]] containing data of a native primitive data type.
 * @param rawData the actual raw data contained in the array
 * @tparam S the type of the contained data.
 */
final class PrimitiveScalarArray[S <: AnyVal: ClassTag](rawData: Array[S]) extends AbstractScalarArray[S, S](rawData) {

  /**
   * Convert one datum from the underlying type to the [[ScalarArray]]'s type. Since for primitive
   * scalars, the underlying data type is the same as the array's data type, the input value is returned unchanged.
   * @param u a value of this array's data type
   * @return the value, unchanged
   */
  override protected def fromUnderlying(u: S): S = u

  /**
   * Returns the <code>index</code>th element of the array
   * @param index the index of the value to return
   * @return the value at index <code>index</code>
   */
  override def apply(index: Int): S = rawData(index)

  /**
   * Returns an iterator over the array's values.
   * @return an iterator over the array's values.
   */
  override def iterator: Iterator[S] = rawData.iterator

  override lazy val hashCode: Int = rawData.deep.hashCode()

  override def canEqual(that: Any): Boolean = that.isInstanceOf[PrimitiveScalarArray[_]]

  override def equals(that: Any) = {
    that match {
      case a: PrimitiveScalarArray[_] => (this canEqual that) && this.rawData.deep == a.rawData.deep
      case _ => false
    }
  }

}

/**
 * A [[ScalarArray]] containing data of a value class type, which can be mapped from/to an underlying primitive data type.
 * @param rawData the actual raw data contained in the array
 * @param scalar a [[Scalar]] instance, providing the necessary data conversion functions
 * @tparam S the type of the array's data
 * @tparam U the type of the underlying contained raw data
 */
final class ValueClassScalarArray[S <: AnyVal, U <: AnyVal](rawData: Array[U])(implicit scalar: ValueClassScalar[S, U]) extends AbstractScalarArray[S, U](rawData) {

  override protected def fromUnderlying(u: U): S = scalar.fromUnderlying(u)

  /**
   * Returns the <code>index</code>th element of the array
   * @param index the index of the value to return
   * @return the value at index <code>index</code>
   */
  override def apply(index: Int): S = fromUnderlying(rawData(index))

  /**
   * Returns an iterator over the array's values.
   * @return an iterator over the array's values.
   */
  override def iterator: Iterator[S] = rawData.iterator.map(scalar.fromUnderlying)

  override lazy val hashCode: Int = rawData.deep.hashCode()

  override def canEqual(that: Any): Boolean = that.isInstanceOf[ValueClassScalarArray[_, _]]

  override def equals(that: Any) = {
    that match {
      case a: ValueClassScalarArray[_, _] => (this canEqual that) && this.rawData.deep == a.rawData.deep
      case _ => false
    }
  }

}

/** Factory for ValueClassScalarArray instances. */
object ValueClassScalarArray {
  def apply[S <: AnyVal, U <: AnyVal](array: Array[U])(implicit s: ValueClassScalar[S, U]): ValueClassScalarArray[S, U] = new ValueClassScalarArray[S, U](array)(s)
}

/** Factory for ScalarArray instances. */
object ScalarArray {

  //  /**
  //   * Converts a native array of scalar values to the corresponding [[ScalarArray]] instance
  //   * @param array a native array of scalar values
  //   * @tparam T the type of the scalar data
  //   * @return the corresponding [[ScalarArray]] instance, containing the same data as <code>array</code>
  //   */
  def apply[T: Scalar: ClassTag](array: Array[T]): ScalarArray[T] = {
    val scalar = implicitly[Scalar[T]]
    scalar match {
      case p: PrimitiveScalar[T] => p.createArray(array)
      case v: ValueClassScalar[T, _] => v.convertArray[T](array, { t => t })
    }
  }

}
