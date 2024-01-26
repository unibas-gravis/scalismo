package scalismo.mesh.decimate

private[decimate] class SymmetricMatrix private (val m: Array[Double]) {

  def add(rhs: SymmetricMatrix): SymmetricMatrix = {
    SymmetricMatrix(
      m(0) + rhs.m(0),
      m(1) + rhs.m(1),
      m(2) + rhs.m(2),
      m(3) + rhs.m(3),
      m(4) + rhs.m(4),
      m(5) + rhs.m(5),
      m(6) + rhs.m(6),
      m(7) + rhs.m(7),
      m(8) + rhs.m(8),
      m(9) + rhs.m(9)
    )
  }
}

private[decimate] object SymmetricMatrix {
  def apply(c: Double): SymmetricMatrix = new SymmetricMatrix(Array.fill(10)(c))

  def apply(m11: Double,
            m12: Double,
            m13: Double,
            m14: Double,
            m22: Double,
            m23: Double,
            m24: Double,
            m33: Double,
            m34: Double,
            m44: Double
  ): SymmetricMatrix =
    new SymmetricMatrix(Array(m11, m12, m13, m14, m22, m23, m24, m33, m34, m44))

  def apply(a: Double, b: Double, c: Double, d: Double): SymmetricMatrix =
    new SymmetricMatrix(Array(a * a, a * b, a * c, a * d, b * b, b * c, b * d, c * c, c * d, d * d))

  def det(a11: Double,
          a12: Double,
          a13: Double,
          a21: Double,
          a22: Double,
          a23: Double,
          a31: Double,
          a32: Double,
          a33: Double
  ): Double = {
    a11 * a22 * a33 + a13 * a21 * a32 + a12 * a23 * a31 -
      a13 * a22 * a31 - a11 * a23 * a32 - a12 * a21 * a33
  }
}
