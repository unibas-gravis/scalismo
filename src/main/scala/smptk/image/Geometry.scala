package smptk.image


import scala.language.higherKinds
import scala.language.implicitConversions


import breeze.linalg.DenseVector
import scala.reflect.ClassTag

trait CoordVector[@specialized(Double, Float, Int) Scalar] {
  def dimensionality: Int
  def apply(i: Int): Scalar
  def toArray : Array[Scalar] 
}

object Geometry {
	  
  
  case class CoordVector1D[@specialized(Double, Float, Int) Scalar : ClassTag](val p: Scalar) extends CoordVector[Scalar] {
    val dimensionality = 1
    def apply(i: Int) = p
    def isDefinedAt(i: Int) = i == 0
    def toArray = Array(p)
  }

  case class CoordVector2D[@specialized(Double, Float, Int) Scalar : ClassTag](val p1 : Scalar, val p2 : Scalar) extends CoordVector[Scalar] {
    val dimensionality = 2
    def apply(i: Int) = {
      if (i == 0) p1 else p2
    }
    def isDefinedAt(i: Int) = i == 0 || i == 1
    def toArray = Array[Scalar](p1, p2)
  }

   case class CoordVector3D[@specialized(Double, Float, Int) Scalar : ClassTag](p1 :Scalar, p2 : Scalar, p3 : Scalar) extends CoordVector[Scalar] {
    val dimensionality = 3
    def apply(i: Int) = {
      if (i == 0) p1 
      else if (i == 1) p2
      else p3
    }
    def isDefinedAt(i: Int) = i == 0 || i == 1 || i == 2
    def toArray = Array(p1, p2, p3)
  }
  
   
  //type Point = CoordVector[Float]

   type Point1D = CoordVector1D[Double]
  type Point2D = CoordVector2D[Double]
  type Point3D = CoordVector3D[Double]
  type Index1D = CoordVector1D[Int]
  type Index2D = CoordVector2D[Int]
  type Index3D = CoordVector3D[Int]
  type Matrix1D = CoordVector1D[CoordVector1D[Double]]
  type Matrix2D = CoordVector2D[CoordVector2D[Double]]
  type Matrix3D = CoordVector3D[CoordVector3D[Double]]
  
  
  implicit def CoordVecDoubleToDouble(c : CoordVector1D[Double]) = c(0)
  
  implicit def doubleToCoordVec(s : Double) = CoordVector1D(s)
  implicit def intToCoordVec(s : Int) = CoordVector1D(s)
  implicit def floatTupleToCoordVec(t : Tuple2[Float, Float]) = CoordVector2D(t._1, t._2)
  implicit def doubleTupleToCoordVec(t : Tuple2[Double, Double]) = CoordVector2D(t._1, t._2)
  implicit def intTupleToCoordVec(t : Tuple2[Int, Int]) = CoordVector2D(t._1, t._2)
  implicit def floatTupleToCoordVec(t : Tuple3[Float, Float, Float]) = CoordVector3D(t._1, t._2, t._3)
  implicit def doubleTupleToCoordVec(t : Tuple3[Double, Double, Double]) = CoordVector3D(t._1, t._2, t._3)
  implicit def intTupleToCoordVec(t : Tuple3[Int, Int, Int]) = CoordVector3D(t._1, t._2, t._3)

}
