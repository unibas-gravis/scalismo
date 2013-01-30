package smptk.image

import breeze.linalg.DenseVector

trait Domain[CoordVector <: CoordVectorLike] { 
  def dimensionality : Int
  def isDefinedAt()
}

trait DiscreteDomain[CoordVector <: CoordVectorLike] extends Domain[CoordVector] {
  def points: IndexedSeq[CoordVector]
  def neighbors(pt : CoordVector) : IndexedSeq[CoordVector]
}
 
trait ContinuousDomain[CoordVector <: CoordVectorLike] extends Domain[CoordVector]{ 
  def uniformSamples(n: Int): IndexedSeq[CoordVector]   
}


trait ContinuousImageDomain[CoordVector <: CoordVectorLike] extends ContinuousDomain[CoordVector]{
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
      val result:Array[CoordVector2D] = new Array[CoordVector2D](n*n)
      var i = 0
      var j = 0
      while(i<n){
        while(j<n){
              result(i*j) = CoordVector2D( DenseVector[Float](origin(0) + i * spacing, origin(1) + j * spacing))    // need to agree on sampling 
          j=j+1
        }
        i=i+1
      }
      result 
    // (0 until n) map (i =>  (0 until n) map (j =>  CoordVector2D( DenseVector[Float](origin(0) + i * spacing, origin(1) + j * spacing)) ) )      // hope this works
    }
}


trait DiscreteImageDomain[CoordVector  <: CoordVectorLike] extends DiscreteDomain[CoordVector] {//extends ImageDomain[Point] {
  def origin : CoordVector
  def spacing: CoordVector
  def size: IndexedSeq[Int]

 }


case class DiscreteImageDomain1D(val origin: CoordVector1D, val extent: CoordVector1D) extends DiscreteImageDomain[CoordVector1D]{
  
}
case class DiscreteImageDomain2D(val origin: CoordVector2D, val extent: CoordVector2D) extends DiscreteImageDomain[CoordVector2D]{}