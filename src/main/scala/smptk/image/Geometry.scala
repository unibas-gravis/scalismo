package smptk.image

import breeze.linalg.DenseVector
import scala.language.higherKinds

trait CoordVectorLike[Scalar] extends PartialFunction[Int, Scalar] {
  def dimensionality: Int
  def apply(i: Int): Scalar
}

object Geometry {
	  
  
  case class CoordVector1D[Scalar](val p: Scalar) extends CoordVectorLike[Scalar] {
    def dimensionality = 1
    def apply(i: Int) = p
    def isDefinedAt(i: Int) = i == 0
  }

  case class CoordVector2D[Scalar](val p: Tuple2[Scalar, Scalar]) extends CoordVectorLike[Scalar] {
    def dimensionality = 2
    def apply(i: Int) = {
      require(i >= 0 && i <= 1)
      if (i == 0) p._1 else p._2
    }
    def isDefinedAt(i: Int) = i == 0 || i == 1
  }

   case class CoordVector3D[Scalar](val p: Tuple3[Scalar, Scalar, Scalar]) extends CoordVectorLike[Scalar] {
    def dimensionality = 3
    def apply(i: Int) = {
      require(i >= 0 && i <= 2)
      if (i == 0) p._1 
      else if (i == 1) p._2
      else p._3
    }
    def isDefinedAt(i: Int) = i == 0 || i == 1 || i == 2
  }

  type PointLike = CoordVectorLike[Float]
  type Point1D = CoordVector1D[Float]
  type Point2D = CoordVector2D[Float]
  type Index1D = CoordVector1D[Int]
  type Index2D = CoordVector2D[Int]
  
  implicit def CoordVecFloatToFloat(c : CoordVector1D[Float]) = c(0)
  
  implicit def floatToCoordVec(s : Float) = CoordVector1D(s)
  implicit def doubleToCoordVec(s : Double) = CoordVector1D(s)
  implicit def intToCoordVec(s : Int) = CoordVector1D(s)
  implicit def floatTupleToCoordVec(t : Tuple2[Float, Float]) = CoordVector2D(t)
  implicit def doubleTupleToCoordVec(t : Tuple2[Double, Double]) = CoordVector2D(t)
  implicit def intTupleToCoordVec(t : Tuple2[Int, Int]) = CoordVector2D(t)
  implicit def floatTupleToCoordVec(t : Tuple3[Float, Float, Float]) = CoordVector3D(t)
  implicit def doubleTupleToCoordVec(t : Tuple3[Double, Double, Double]) = CoordVector3D(t)
  implicit def intTupleToCoordVec(t : Tuple3[Int, Int, Int]) = CoordVector3D(t)

}
