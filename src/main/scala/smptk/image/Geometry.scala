package smptk.image

import breeze.linalg.DenseVector
import scala.language.higherKinds


trait CoordVectorLike[Scalar] extends PartialFunction[Int, Scalar] {
  def dimensionality: Int
  def apply(i: Int): Scalar
}

object Geometry { 
	
	implicit class CoordVector1D[Scalar](val p: Scalar) extends CoordVectorLike[Scalar] {
	  def dimensionality = 1
	  def apply(i: Int) = p
	  def isDefinedAt(i : Int) = i == 0  
	}
	
	implicit class CoordVector2D[Scalar](val p : Tuple2[Scalar, Scalar]) extends CoordVectorLike[Scalar] {
	  def dimensionality = 2
	  def apply(i: Int) = {
	    require (i >= 0 && i <= 1)
	    if (i == 0) p._1 else p._2
	  }
	  def isDefinedAt(i : Int) = i == 0 || i == 1
	}
	type PointLike = CoordVectorLike[Float]
	type Point1D = CoordVector1D[Float]
	type Point2D = CoordVector2D[Float]
	type Index1D = CoordVector1D[Int]
	type Index2D = CoordVector2D[Int]
}
