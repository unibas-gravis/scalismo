package smptk.image

import breeze.linalg.DenseVector
import smptk.image.Geometry._


trait Domain[CoordVector[A] <: CoordVectorLike[A]] {
  def dimensionality: Int
}

trait ContinuousDomain[CoordVector[A] <: CoordVectorLike[A]] extends Domain[CoordVector] {
  def uniformSamples(n: Int): IndexedSeq[CoordVector[Float]]
}

trait ContinuousImageDomain[CoordVector[A] <: CoordVectorLike[A]] extends ContinuousDomain[CoordVector] {
  def origin: CoordVector[Float]
  def extent: CoordVector[Float]
  def isInside(pt: CoordVector[Float]): Boolean
}

case class ContinuousImageDomain1D(val origin: CoordVector1D[Float], val extent: CoordVector1D[Float]) extends ContinuousImageDomain[CoordVector1D] {
  def dimensionality = 1
  def isInside(pt: CoordVector1D[Float]): Boolean = pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0)

  def uniformSamples(n: Int) = {
    val spacing: Float = extent(0) / n
    (0 until n) map (i => CoordVector1D(origin(0) + i * spacing))
  }
}

case class ContinuousImageDomain2D(val origin: Point2D, val extent: Point2D) extends ContinuousImageDomain[CoordVector2D] {
  def dimensionality = 2
  def isInside(pt: Point2D): Boolean = pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0) && pt(1) >= origin(1) && pt(1) <= origin(1) + extent(1)

  def uniformSamples(n: Int) = {
    val spacing: Float = extent(0) / n
    val result: Array[Point2D] = new Array[Point2D](n * n)
    var i = 0
    var j = 0
    while (i < n) {
      while (j < n) {
        result(i * j) = (origin(0) + i * spacing, origin(1) + j * spacing) // need to agree on sampling 
        j = j + 1
      }
      i = i + 1
    }
    result
    // (0 until n) map (i =>  (0 until n) map (j =>  CoordVector2D( DenseVector[Float](origin(0) + i * spacing, origin(1) + j * spacing)) ) )      // hope this works
  }
}

trait DiscreteDomain[CoordVector[A] <: CoordVectorLike[A]] extends Domain[CoordVector] {
  def points: IndexedSeq[CoordVector[Float]]
  //def neighbors(pt: CoordVector): IndexedSeq[CoordVector]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}

trait DiscreteImageDomain[CoordVector[A] <: CoordVectorLike[A]] extends DiscreteDomain[CoordVector] { //extends ImageDomain[Point] {
  def origin: CoordVector[Float]
  def spacing: CoordVector[Float]
  def size: CoordVector[Int]
  def extent : CoordVector[Float]
}

case class DiscreteImageDomain1D(val origin: CoordVector1D[Float], val spacing: CoordVector1D[Float], val size : CoordVector1D[Int]) extends DiscreteImageDomain[CoordVector1D] {
  def dimensionality = 1
  def points = {
	for (i <- 0 until size(0)) yield CoordVector1D(origin(0) + spacing(0) * i)	  
  }
  def extent = CoordVector1D(origin(0) + spacing(0) * size(0))

}
case class DiscreteImageDomain2D(val origin: CoordVector2D[Float], val spacing: CoordVector2D[Float], val size : CoordVector2D[Int]) extends DiscreteImageDomain[CoordVector2D] {
  def dimensionality = 2  
  def points = {
	for (i <- 0 until size(0); j <- 0 until size(1)) 
	  yield CoordVector2D(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j)	  
    }
  
  def extent = CoordVector2D(origin(0) + spacing(0) * size(0), origin(1) + spacing(1) * size(2))
  
  
}