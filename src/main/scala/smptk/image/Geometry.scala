package smptk.image

import breeze.linalg.DenseVector




trait CoordVectorLike extends PartialFunction[Int, Float] {
  def dimensionality: Int
  def apply(i: Int): Float 
}

object Geometry { 
	
	implicit class CoordVector1D(val p: Float) extends CoordVectorLike {
	  def dimensionality = 1
	  def apply(i: Int) = p
	  def isDefinedAt(i : Int) = i == 0  
	}
	
	implicit class CoordVector2D(val p : Tuple2[Float, Float]) extends CoordVectorLike {
	  def dimensionality = 2
	  def apply(i: Int) = {
	    require (i >= 0 && i <= 1)
	    if (i == 0) p._1 else p._2
	  }
	  def isDefinedAt(i : Int) = i == 0 || i == 1
	}
}
