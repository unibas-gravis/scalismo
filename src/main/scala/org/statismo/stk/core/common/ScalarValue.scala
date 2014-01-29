package org.statismo.stk.core.common



/** typeclass defining type conversions betwen different pixel types*/
trait ScalarValue[@specialized(Short, Float, Double) T] {
  def fromDouble(d: Double): T
  def fromFloat(f: Float): T
  def fromShort(s: Short): T
  def fromInt(s: Int) : T
  def toDouble(t: T): Double
  def toFloat(t: T): Float
  def toShort(t: T): Short
  def toInt(t : T) : Int
}


object ScalarValue {
  implicit val pixelFloatConversions = new ScalarValue[Float] {
    def fromDouble(d: Double) = d.toFloat
    def fromFloat(f: Float) = f
    def fromShort(s: Short) = s.toFloat
    def fromInt(s: Int) = s.toInt
    def toDouble(t: Float) = t.toDouble
    def toFloat(t: Float) = t
    def toShort(t: Float) = t.toShort
    def toInt(t : Float) = t.toInt
  }

  implicit val pixelShortConversions = new ScalarValue[Short] {
    def fromDouble(d: Double) = d.toShort
    def fromFloat(f: Float) = f.toShort
    def fromShort(s: Short) = s
    def fromInt(i : Int) = i.toShort
    def toDouble(t: Short) = t.toDouble
    def toFloat(t: Short) = t
    def toShort(t: Short) = t.toShort
    def toInt(t : Short) = t.toInt
  }

  implicit val pixelDoubleConversions = new ScalarValue[Double] {
    def fromDouble(d: Double) = d
    def fromFloat(f: Float) = f.toDouble
    def fromShort(s: Short) = s.toDouble
    def fromInt(i : Int) = i.toDouble
    def toDouble(t: Double) = t
    def toFloat(t: Double) = t.toFloat
    def toShort(t: Double) = t.toShort
    def toInt(t: Double) = t.toInt
  }
  
  implicit val pixelIntConversions = new ScalarValue[Int] {
    def fromDouble(d: Double) = d.toInt
    def fromFloat(f: Float) = f.toInt
    def fromShort(s: Short) = s.toInt
    def fromInt(i : Int) = i
    def toDouble(i : Int) = i.toDouble
    def toFloat(i : Int) = i.toFloat
    def toShort(i : Int) = i.toShort
    def toInt(i : Int) = i
  }
  
}
