package smptk.image


import scala.language.higherKinds
import scala.language.implicitConversions


import breeze.linalg.DenseVector
import scala.reflect.ClassTag

trait CoordVector[Scalar] extends PartialFunction[Int, Scalar] {
  def dimensionality: Int
  def apply(i: Int): Scalar
  def toArray : Array[Scalar] 
}

object Geometry {
	  
  
  case class CoordVector1D[Scalar : ClassTag](val p: Scalar) extends CoordVector[Scalar] {
    def dimensionality = 1
    def apply(i: Int) = p
    def isDefinedAt(i: Int) = i == 0
    def toArray = Array(p)
  }

  case class CoordVector2D[Scalar : ClassTag](val p: Tuple2[Scalar, Scalar]) extends CoordVector[Scalar] {
    def dimensionality = 2
    def apply(i: Int) = {
      require(i >= 0 && i <= 1)
      if (i == 0) p._1 else p._2
    }
    def isDefinedAt(i: Int) = i == 0 || i == 1
    def toArray = Array[Scalar](p._1, p._2)
  }

   case class CoordVector3D[Scalar : ClassTag](val p: Tuple3[Scalar, Scalar, Scalar]) extends CoordVector[Scalar] {
    def dimensionality = 3
    def apply(i: Int) = {
      require(i >= 0 && i <= 2)
      if (i == 0) p._1 
      else if (i == 1) p._2
      else p._3
    }
    def isDefinedAt(i: Int) = i == 0 || i == 1 || i == 2
    def toArray = Array(p._1, p._2, p._3)
  }

  type PointLike = CoordVector[Float]
  type Point1D = CoordVector1D[Float]
  type Point2D = CoordVector2D[Float]
  type Point3D = CoordVector3D[Float]
  type Index1D = CoordVector1D[Int]
  type Index2D = CoordVector2D[Int]
  type Index3D = CoordVector3D[Int]
  
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
