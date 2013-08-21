package smptk

package object image {
  implicit val pixelFloatConversions = new ScalarPixel[Float] {
    def fromDouble(d: Double) = d.toFloat
    def fromFloat(f: Float) = f
    def fromShort(s: Short) = s.toFloat
    def fromInt(i: Int) = i.toFloat
    def toDouble(t: Float) = t.toDouble
    def toFloat(t: Float) = t
    def toShort(t: Float) = t.toShort
    def toInt(t: Float) = t.toInt
  }

  implicit val pixelShortConversions = new ScalarPixel[Short] {
    def fromDouble(d: Double) = d.toShort
    def fromFloat(f: Float) = f.toShort
    def fromShort(s: Short) = s
    def fromInt(i: Int) = i.toShort
    def toDouble(t: Short) = t.toDouble
    def toFloat(t: Short) = t.toFloat
    def toShort(t: Short) = t
    def toInt(t: Short) = t.toInt
  }

  implicit val pixelDoubleConversions = new ScalarPixel[Double] {
    def fromDouble(d: Double) = d
    def fromFloat(f: Float) = f.toDouble
    def fromShort(s: Short) = s.toDouble
    def fromInt(i: Int) = i.toDouble
    def toDouble(t: Double) = t
    def toFloat(t: Double) = t.toFloat
    def toShort(t: Double) = t.toShort
    def toInt(t: Double) = t.toInt
  }

  implicit val pixelIntConversions = new ScalarPixel[Int] {
    def fromDouble(d: Double) = d.toInt
    def fromFloat(f: Float) = f.toInt
    def fromShort(s: Short) = s.toInt
    def fromInt(i: Int) = i.toInt
    def toDouble(t: Int) = t.toDouble
    def toFloat(t: Int) = t.toFloat
    def toShort(t: Int) = t.toShort
    def toInt(t: Int) = t.toInt
  }

}