package smptk
package common

import geometry.{Point, Vector, Dim, OneD, TwoD, ThreeD}
import breeze.linalg.DenseVector
import scala.collection.SeqView


trait Domain[D <: Dim] {
  def dimensionality: Int
}

trait Cell { 
  def pointIds : IndexedSeq[Int]
}


trait DiscreteDomain[D <: Dim] extends Domain[D] {
  def points: SeqView[Point[D],Seq[_]]
  // def cells : IndexedSeq[Cell] // TODO add it later here
  def numberOfPoints: Int

  //def neighbors(pt: Dim): IndexedSeq[Dim]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}


trait BoxedRegion[D <: Dim] extends Domain[D] {
  def origin: Point[D]
  def extent: Point[D]
  def volume : Double = (0 until dimensionality).foldLeft(1.)((prod, i) => prod * (extent(i) - origin(i)))
}

case class BoxedRegion1D( originV : Point[OneD], extentV: Point[OneD]) extends BoxedRegion[OneD] {
  def dimensionality = 1
  def origin = originV
  def extent = extentV
}

case class BoxedRegion2D(originV : Point[TwoD], extentV: Point[TwoD]) extends BoxedRegion[TwoD] {
  def dimensionality = 2
  def origin = originV
  def extent = extentV
}

case class BoxedRegion3D(originV : Point[ThreeD], extentV: Point[ThreeD]) extends BoxedRegion[ThreeD] {
  def dimensionality = 3
  def origin = originV
  def extent = extentV  
}

