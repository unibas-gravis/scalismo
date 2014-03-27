package org.statismo.stk.core
package object geometry {
  // additional information associate to the DIM Type
  sealed trait DimTraits[D <: Dim] {
    def dimensionality: Int
    def zeroVector: Vector[D]
    def zeroPoint : Point[D]
    def createVector(d: Array[Float]): Vector[D]
    def createPoint(d: Array[Float]): Point[D]
    def createMatrixNxN(d : Array[Float]) : MatrixNxN[D]
    def zeroMatrix : MatrixNxN[D]
    def eye : MatrixNxN[D]
  }

  implicit val oneD = new DimTraits[OneD] {
    def dimensionality = 1
    def zeroVector = Vector1D(0f)
    def zeroPoint = Point1D(0f)

    def createPoint(d: Array[Float]) = {
     if (d.size != 1)
        throw new Exception(s"Require array of size 1 to create a Point1D (got ${d.size}")
      Point1D(d(0))
    }

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

    def zeroMatrix = Matrix1x1.zeros
    def eye = Matrix1x1.eye

  }
  implicit val twoD = new DimTraits[TwoD] {
    def dimensionality = 2
    def zeroVector = Vector2D(0f, 0f)
    def zeroPoint = Point2D(0f, 0f)

    def createVector(d: Array[Float]) = {
      if (d.size != 2)
        throw new Exception(s"Require array of size 1 to create a Vector2D (got ${d.size}")
      Vector2D(d(0), d(1))
    }

    def createPoint(d: Array[Float]) = {
      if (d.size != 2)
        throw new Exception(s"Require array of size 1 to create a Point2D (got ${d.size}")
      Point2D(d(0), d(1))
    }


    def createMatrixNxN(d: Array[Float]) = {
      if (d.size != 4) {
        throw new Exception(s"Require array of size 4 to create a Matrix2x2 (got ${d.size}")
      }
      Matrix2x2(d)
    }

    def zeroMatrix = Matrix2x2.zeros
    def eye = Matrix2x2.eye


  }
  implicit val threeD = new DimTraits[ThreeD] {
    def dimensionality = 3
    def zeroVector = Vector3D(0f, 0f, 0f)
    def zeroPoint = Point3D(0f, 0f, 0f)

    def createVector(d: Array[Float]) = {
      if (d.size != 3)
        throw new Exception(s"Require array of size 1 to create a Vector3D (got ${d.size}")
      Vector3D(d(0), d(1), d(2))
    }

    def createPoint(d: Array[Float]) = {
      if (d.size != 3)
        throw new Exception(s"Require array of size 1 to create a Point3D (got ${d.size}")
      Point3D(d(0), d(1), d(2))
    }


    def createMatrixNxN(d: Array[Float]) = {
      if (d.size != 9) {
        throw new Exception(s"Require array of size 9 to create a Matrix3x3 (got ${d.size}")
      }
      Matrix3x3(d)
    }

    def zeroMatrix = Matrix3x3.zeros
    def eye = Matrix3x3.eye

  }


}