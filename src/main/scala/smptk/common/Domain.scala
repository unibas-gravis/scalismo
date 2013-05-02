package smptk
package common


import smptk.image.Geometry._
import smptk.image.CoordVector
import breeze.linalg.DenseVector
import scala.collection.SeqView


trait Domain[CV[A] <: CoordVector[A]] {
  def dimensionality: Int
}

trait Cell { 
  def pointIds : IndexedSeq[Int]
}


trait DiscreteDomain[CV[A] <: CoordVector[A]] extends Domain[CV] {
  def points: SeqView[CV[Double],Seq[_]]
  // def cells : IndexedSeq[Cell] // TODO add it later here
  def numberOfPoints: Int

  //def neighbors(pt: CoordVector): IndexedSeq[CoordVector]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}


trait BoxedRegion[CV[A] <: CoordVector[A]] extends Domain[CV] {
  def origin: CV[Double]
  def extent: CV[Double]
  def volume : Double = (0 until dimensionality).foldLeft(1.)((prod, i) => prod * (extent(i) - origin(i)))
}

case class BoxedRegion1D( originV : Point1D, extentV: Point1D) extends BoxedRegion[CoordVector1D] {
  def dimensionality = 1
  def origin = originV
  def extent = extentV
}

case class BoxedRegion2D(originV : Point2D, extentV: Point2D) extends BoxedRegion[CoordVector2D] {
  def dimensionality = 2
  def origin = originV
  def extent = extentV
}

case class BoxedRegion3D(originV : Point3D, extentV: Point3D) extends BoxedRegion[CoordVector3D] {
  def dimensionality = 3
  def origin = originV
  def extent = extentV  
}

