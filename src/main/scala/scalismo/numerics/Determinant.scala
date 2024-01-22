package scalismo.numerics

object Determinant {

  @inline
  def det2x2(a1: Double, a2: Double, b1: Double, b2: Double): Double = {
    (+a1 * b2
      - b1 * a2)
  }

  @inline
  def det3x3(a1: Double, a2: Double, a3: Double, b1: Double, b2: Double, b3: Double, c1: Double, c2: Double, c3: Double)
    : Double = {
    (+a1 * det2x2(b2, b3, c2, c3)
      - b1 * det2x2(a2, a3, c2, c3)
      + c1 * det2x2(a2, a3, b2, b3))
  }

  @inline
  def det4x4(a1: Double,
             a2: Double,
             a3: Double,
             a4: Double,
             b1: Double,
             b2: Double,
             b3: Double,
             b4: Double,
             c1: Double,
             c2: Double,
             c3: Double,
             c4: Double,
             d1: Double,
             d2: Double,
             d3: Double,
             d4: Double
  ): Double = {
    (+a1 * det3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4)
      - b1 * det3x3(c2, c3, c4, d2, d3, d4, a2, a3, a4)
      + c1 * det3x3(d2, d3, d4, a2, a3, a4, b2, b3, b4)
      - d1 * det3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4)
      - a2 * det3x3(b3, b4, b1, c3, c4, c1, d3, d4, d1)
      + b2 * det3x3(c3, c4, c1, d3, d4, d1, a3, a4, a1)
      - c2 * det3x3(d3, d4, d1, a3, a4, a1, b3, b4, b1)
      + d2 * det3x3(a3, a4, a1, b3, b4, b1, c3, c4, c1)
      + a3 * det3x3(b4, b1, b2, c4, c1, c2, d4, d1, d2)
      - b3 * det3x3(c4, c1, c2, d4, d1, d2, a4, a1, a2)
      + c3 * det3x3(d4, d1, d2, a4, a1, a2, b4, b1, b2)
      - d3 * det3x3(a4, a1, a2, b4, b1, b2, c4, c1, c2)
      - a4 * det3x3(b1, b2, b3, c1, c2, c3, d1, d2, d3)
      + b4 * det3x3(c1, c2, c3, d1, d2, d3, a1, a2, a3)
      - c4 * det3x3(d1, d2, d3, a1, a2, a3, b1, b2, b3)
      + d4 * det3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3))
  }
}
