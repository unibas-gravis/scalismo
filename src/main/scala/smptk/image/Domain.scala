package smptk.image

import breeze.linalg.DenseVector
import smptk.image.Geometry.CoordVector1D
import smptk.image.Geometry.CoordVector2D

trait Domain[CoordVector <: CoordVectorLike] {
  def dimensionality: Int
}

trait ContinuousDomain[CoordVector <: CoordVectorLike] extends Domain[CoordVector] {
  def uniformSamples(n: Int): IndexedSeq[CoordVector]
}

trait ContinuousImageDomain[CoordVector <: CoordVectorLike] extends ContinuousDomain[CoordVector] {
  def origin: CoordVector
  def extent: CoordVector
  def isInside(pt: CoordVector): Boolean
}

case class ContinuousImageDomain1D(val origin: CoordVector1D, val extent: CoordVector1D) extends ContinuousImageDomain[CoordVector1D] {
  def dimensionality = 1
  def isInside(pt: CoordVector1D): Boolean = pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0)

  def uniformSamples(n: Int) = {
    val spacing: Float = extent(0) / n
    (0 until n) map (i => CoordVector1D(origin(0) + i * spacing))
  }
}

case class ContinuousImageDomain2D(val origin: CoordVector2D, val extent: CoordVector2D) extends ContinuousImageDomain[CoordVector2D] {
  def dimensionality = 2
  def isInside(pt: CoordVector2D): Boolean = pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0) && pt(1) >= origin(1) && pt(1) <= origin(1) + extent(1)

  def uniformSamples(n: Int) = {
    val spacing: Float = extent(0) / n
    val result: Array[CoordVector2D] = new Array[CoordVector2D](n * n)
    var i = 0
    var j = 0
    while (i < n) {
      while (j < n) {
        result(i * j) = CoordVector2D(origin(0) + i * spacing, origin(1) + j * spacing) // need to agree on sampling 
        j = j + 1
      }
      i = i + 1
    }
    result
    // (0 until n) map (i =>  (0 until n) map (j =>  CoordVector2D( DenseVector[Float](origin(0) + i * spacing, origin(1) + j * spacing)) ) )      // hope this works
  }
}

trait DiscreteDomain[CoordVector <: CoordVectorLike] extends Domain[CoordVector] {
  def points: IndexedSeq[CoordVector]
  //def neighbors(pt: CoordVector): IndexedSeq[CoordVector]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}

trait DiscreteImageDomain[CoordVector <: CoordVectorLike] extends DiscreteDomain[CoordVector] { //extends ImageDomain[Point] {
  def origin: CoordVector
  def spacing: CoordVector
  def size: Product
  def extent : CoordVector
}

case class DiscreteImageDomain1D(val origin: CoordVector1D, val spacing: CoordVector1D, val size : Tuple1[Int]) extends DiscreteImageDomain[CoordVector1D] {
  def dimensionality = 1
  def points = {
	for (i <- 0 until size._1) yield CoordVector1D(origin(0) + spacing(0) * i)	  
  }
  def extent = CoordVector1D(origin(0) + spacing(0) * size._1)

}
case class DiscreteImageDomain2D(val origin: CoordVector2D, val spacing: CoordVector2D, val size : Tuple2[Int, Int]) extends DiscreteImageDomain[CoordVector2D] {
  def dimensionality = 2  
  def points = {
	for (i <- 0 until size._1; j <- 0 until size._2) 
	  yield CoordVector2D(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j)	  
    }
  
  def extent = CoordVector2D(origin(0) + spacing(0) * size._1, origin(1) + spacing(1) * size._2)
  
  
}