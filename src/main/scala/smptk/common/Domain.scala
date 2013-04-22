package smptk
package common


import smptk.image.Geometry._
import smptk.image.CoordVector
import breeze.linalg.DenseVector


trait Domain[CV[A] <: CoordVector[A]] {
  def dimensionality: Int
}

trait Cell { 
  def pointIds : IndexedSeq[Int]
}


trait DiscreteDomain[CV[A] <: CoordVector[A]] extends Domain[CV] {
  def points: IndexedSeq[CV[Double]]
  // def cells : IndexedSeq[Cell] // TODO add it later here
  def numberOfPoints: Int

  //def neighbors(pt: CoordVector): IndexedSeq[CoordVector]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}


trait BoxedRegion[CV[A] <: CoordVector[A]] extends Domain[CV] {
  def origin: CV[Double]
  def extent: CV[Double]
  
  def uniformSamples(n: Int): IndexedSeq[CV[Double]]
  def uniformDistributionRandomSamples(n :Int) : IndexedSeq[CV[Double]]
}

case class BoxedRegion1D( originV : Point1D, extentV: Point1D) extends BoxedRegion[CoordVector1D] {
  def dimensionality = 1
  def origin = originV
  def extent = extentV
  
  def uniformSamples(n: Int): IndexedSeq[Point1D] = {
    val step = (extent(0) - origin(0)) / n
    for (i <- 0 until n) yield CoordVector1D[Double](origin(0) + i * step)
  }

  
  // Note : the points returned here are not necessarily on the grid
   def uniformDistributionRandomSamples(n :Int) : IndexedSeq[Point1D] = {
	 val distr = breeze.stats.distributions.Uniform(origin(0), extent(0))
	 (0 until n).map(i=> CoordVector1D(distr.draw)) 
   }
}
case class BoxedRegion2D(originV : Point2D, extentV: Point2D) extends BoxedRegion[CoordVector2D] {
  def dimensionality = 2
  def origin = originV
  def extent = extentV
  def uniformSamples(n: Int): IndexedSeq[CoordVector2D[Double]] = {
    val nbPerDim = math.sqrt(n).floor.toInt
    val step0 = (extent(0) - origin(0)) / nbPerDim
    val step1 = (extent(1) - origin(1)) / nbPerDim
    for (i <- 0 until nbPerDim; j <- 0 until nbPerDim) yield CoordVector2D[Double](origin(0) + i * step0, origin(1) + j * step1)
  }
  
    // Note : the points returned here are not necessarily on the grid
   def uniformDistributionRandomSamples(n :Int) : IndexedSeq[Point2D] = {
	 val distrDim1 = breeze.stats.distributions.Uniform(origin(0), extent(0))
	 val distrDim2 = breeze.stats.distributions.Uniform(origin(1), extent(1))
	
	 (0 until n).map(i=> CoordVector2D(distrDim1.draw, distrDim2.draw)) 
   }
}
