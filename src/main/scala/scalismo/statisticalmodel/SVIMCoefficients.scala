package scalismo.statisticalmodel

import breeze.linalg.DenseVector

case class SVIMCoefficients(shape: DenseVector[Double],
    intensity: DenseVector[Double]) {
  def *(f: Float): SVIMCoefficients = this * f.toDouble
  def *(d: Double): SVIMCoefficients = copy(shape = shape * d, intensity = intensity * d)
  def +(other: SVIMCoefficients): SVIMCoefficients = copy(shape = shape + other.shape, intensity = intensity + other.intensity)
  def -(other: SVIMCoefficients): SVIMCoefficients = copy(shape = shape - other.shape, intensity = intensity - other.intensity)
}

object SVIMCoefficients {
  def apply(shape: IndexedSeq[Double],
    color: IndexedSeq[Double]) = new SVIMCoefficients(DenseVector(shape.toArray), DenseVector(color.toArray))

  def apply(shape: DenseVector[Double],
    color: DenseVector[Double]) = new SVIMCoefficients(shape, color)

  /** get 0 coefficients of specified length */
  def zeros(shapeComponents: Int,
    colorComponents: Int): SVIMCoefficients = {
    new SVIMCoefficients(
      DenseVector.zeros(shapeComponents),
      DenseVector.zeros(colorComponents))
  }
}