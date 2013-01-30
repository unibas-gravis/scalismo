package smptk.image

import breeze.linalg.DenseVector




trait CoordVectorLike extends PartialFunction[Int, Float] {
  def dimensionality: Int
  def apply(i: Int): Float 
}

class CoordVector1D(val p: Float) extends CoordVectorLike {
  def dimensionality = 1
  def apply(i: Int) = p
  def isDefinedAt(i : Int) = i == 0  
}

case class CoordVector2D(val p1 : Float, p2 : Float) extends CoordVectorLike {
  def dimensionality = 2
  def apply(i: Int) = {
    require (i >= 0 && i <= 1)
    if (i == 0) p1 else p2    
  }
  def isDefinedAt(i : Int) = i == 0 || i == 1
}

