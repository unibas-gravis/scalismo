package smptk
package object geometry {
// additional information associate to the DIM Type
sealed trait DimTraits[D <: Dim] {
  def dimensionality: Int
}

  implicit val oneD = new DimTraits[OneD] {
    def dimensionality = 1
  }
  implicit val twoD = new DimTraits[TwoD] {
    def dimensionality = 2
  }
  implicit val threeD = new DimTraits[ThreeD] {
    def dimensionality = 3
  }
}