package smptk
package object geometry {
  // additional information associate to the DIM Type
  sealed trait DimTraits[D <: Dim] {
    def dimensionality: Int
    def zeroVector: Vector[D]
    def createVector(d: Array[Float]): Vector[D]
    def createMatrixNxN(d : Array[Float]) : MatrixNxN[D]
  }

  implicit val oneD = new DimTraits[OneD] {
    def dimensionality = 1
    def zeroVector = Vector1D(0f)
    def createVector(d: Array[Float]) = {
      if (d.size != 1)
        throw new Exception(s"Require array of size 1 to create a Vector1D (got ${d.size}")
      Vector1D(d(0))
    }

    def createMatrixNxN(d: Array[Float]) = {
      if (d.size != 1) {
        throw new Exception(s"Require array of size 1 to create a Matrix1x1 (got ${d.size}")
      }
      Matrix1x1(d)
    }

  }
  implicit val twoD = new DimTraits[TwoD] {
    def dimensionality = 2
    def zeroVector = Vector2D(0f, 0f)
    def createVector(d: Array[Float]) = {
      if (d.size != 2)
        throw new Exception(s"Require array of size 1 to create a Vector1D (got ${d.size}")
      Vector2D(d(0), d(1))
    }
    def createMatrixNxN(d: Array[Float]) = {
      if (d.size != 4) {
        throw new Exception(s"Require array of size 4 to create a Matrix2x2 (got ${d.size}")
      }
      Matrix2x2(d)
    }

  }
  implicit val threeD = new DimTraits[ThreeD] {
    def dimensionality = 3
    def zeroVector = Vector3D(0f, 0f, 0f)
    def createVector(d: Array[Float]) = {
      if (d.size != 3)
        throw new Exception(s"Require array of size 1 to create a Vector3D (got ${d.size}")
      Vector3D(d(0), d(1), d(2))
    }
    def createMatrixNxN(d: Array[Float]) = {
      if (d.size != 9) {
        throw new Exception(s"Require array of size 9 to create a Matrix3x3 (got ${d.size}")
      }
      Matrix3x3(d)
    }

  }
}