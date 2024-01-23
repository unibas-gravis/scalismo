package scalismo.mesh.decimate

private[decimate] class SymmetricMatrix private (private val m: Array[Double]) {

  def this(c: Double) = this(Array.fill(10)(c))

  def this(m11: Double,
           m12: Double,
           m13: Double,
           m14: Double,
           m22: Double,
           m23: Double,
           m24: Double,
           m33: Double,
           m34: Double,
           m44: Double
  ) = this(Array(m11, m12, m13, m14, m22, m23, m24, m33, m34, m44))

  // Make plane
  def this(a: Double, b: Double, c: Double, d: Double) =
    this(Array(a * a, a * b, a * c, a * d, b * b, b * c, b * d, c * c, c * d, d * d))

  def getValue(c: Int): Double = m(c)

  // Determinant
  def det(a11: Int, a12: Int, a13: Int, a21: Int, a22: Int, a23: Int, a31: Int, a32: Int, a33: Int): Double = {
    m(a11) * m(a22) * m(a33) + m(a13) * m(a21) * m(a32) + m(a12) * m(a23) * m(a31) -
      m(a13) * m(a22) * m(a31) - m(a11) * m(a23) * m(a32) - m(a12) * m(a21) * m(a33)
  }

  def add(n: SymmetricMatrix): SymmetricMatrix = {
    new SymmetricMatrix(
      m(0) + n.getValue(0),
      m(1) + n.getValue(1),
      m(2) + n.getValue(2),
      m(3) + n.getValue(3),
      m(4) + n.getValue(4),
      m(5) + n.getValue(5),
      m(6) + n.getValue(6),
      m(7) + n.getValue(7),
      m(8) + n.getValue(8),
      m(9) + n.getValue(9)
    )
  }
}

private[decimate] object SymmetricMatrix {
  def apply(c: Double): SymmetricMatrix = new SymmetricMatrix(c)

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
    new SymmetricMatrix(m11, m12, m13, m14, m22, m23, m24, m33, m34, m44)

  def apply(a: Double, b: Double, c: Double, d: Double): SymmetricMatrix =
    new SymmetricMatrix(a, b, c, d)
}
