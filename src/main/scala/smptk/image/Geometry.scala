package smptk.image




trait CoordVectorLike extends PartialFunction[Int, Float] {
  def dimensionality: Int
  def apply(i: Int): Float
}

case class CoordVector1D(val p: Float) extends CoordVectorLike {
  def dimensionality = 1
  def apply(i: Int) = p
}

  

