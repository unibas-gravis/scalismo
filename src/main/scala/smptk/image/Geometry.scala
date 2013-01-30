package smptk.image

import breeze.linalg.DenseVector




trait CoordVectorLike extends PartialFunction[Int, Float] {
  def dimensionality: Int
  def apply(i: Int): Float
}

case class CoordVector1D(val p: Float) extends CoordVectorLike {
  def dimensionality = 1
  def apply(i: Int) = p
}

case class CoordVector2D(val p: DenseVector[Float]) extends CoordVectorLike {
  def dimensionality = 2
  def apply(i: Int) = p(i) 
}

