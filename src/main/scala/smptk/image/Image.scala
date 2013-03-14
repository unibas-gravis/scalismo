package smptk
package image


/** typeclass defining type conversions betwen different pixel types*/
trait ScalarPixel[@specialized(Short, Float, Double) T] {
  def fromDouble(d: Double): T
  def fromFloat(f: Float): T
  def fromShort(s: Short): T
  def toDouble(t: T): Double
  def toFloat(t: T): Float
  def toShort(t: T): Short
}


object Image {
  implicit val pixelFloatConversions = new ScalarPixel[Float] {
    def fromDouble(d: Double) = d.toFloat
    def fromFloat(f: Float) = f
    def fromShort(s: Short) = s.toFloat
    def toDouble(t: Float) = t.toDouble
    def toFloat(t: Float) = t
    def toShort(t: Float) = t.toShort
  }

  implicit val pixelShortConversions = new ScalarPixel[Short] {
    def fromDouble(d: Double) = d.toShort
    def fromFloat(f: Float) = f.toShort
    def fromShort(s: Short) = s
    def toDouble(t: Short) = t.toDouble
    def toFloat(t: Short) = t
    def toShort(t: Short) = t.toShort
  }

  implicit val pixelDoubleConversions = new ScalarPixel[Double] {
    def fromDouble(d: Double) = d
    def fromFloat(f: Float) = f.toDouble
    def fromShort(s: Short) = s.toDouble
    def toDouble(t: Double) = t
    def toFloat(t: Double) = t.toFloat
    def toShort(t: Double) = t.toShort
  }
}
