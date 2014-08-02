package org.statismo.stk.core
package image

import common.{ Domain, DiscreteDomain, BoxedDomain }
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain1D



//case class ContinuousImageDomain1D(val isInside: CoordVector[OneD][Double] => Boolean) extends ContinuousImageDomain[CoordVector[OneD]] {
//  def dimensionality = 1
//
//}
//
//case class ContinuousImageDomain2D(val isInside: CoordVector[TwoD][Double] => Boolean) extends ContinuousImageDomain[CoordVector[TwoD]] {
//  def dimensionality = 2
//}
//
//case class ContinuousImageDomain3D(val isInside: CoordVector[ThreeD][Double] => Boolean) extends ContinuousImageDomain[CoordVector[ThreeD]] {
//  def dimensionality = 3
//}

abstract class DiscreteImageDomain[D <: Dim : NDSpaceOps] extends DiscreteDomain[D] with BoxedDomain[D]{ //extends ImageDomain[Point] {

  def spacing: Vector[D]
  def size: Index[D]

  def directions: Array[Double]

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  def indexToLinearIndex(idx: Index[D]): Int
  def linearIndexToIndex(linearIdx: Int): Index[D]

  def isInside(pt: Point[D]): Boolean



}

case class DiscreteImageDomain1D(val origin: Point[_1D], val spacing: Vector[_1D], val size: Index[_1D]) extends DiscreteImageDomain[_1D]  {


  def points = for (i <- (0 until size(0)).view) yield Point(origin(0) + spacing(0) * i) // TODO replace with operator version

  val extent : Point[_1D] = Point(origin(0) + spacing(0) * size(0))

  def indexToLinearIndex(idx: Index[_1D]) = idx(0)
  def linearIndexToIndex(linearIdx: Int) = Index(linearIdx)

  val directions = Array(1.0)

  def isInside(pt: Point[_1D]): Boolean = {
    pt(0) >= origin(0) && pt(0) <= extent(0)
  }


}

case class DiscreteImageDomain2D(val origin: Point[_2D], val spacing: Vector[_2D], val size: Index[_2D]) extends DiscreteImageDomain[_2D] {


  def points = for (j <- (0 until size(1)).view; i <- (0 until size(0)).view)
    yield Point(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j)

  val extent : Point[_2D] = Point(origin(0) + spacing(0) * size(0), origin(1) + spacing(1) * size(1)) // TODO replace with generic operator version

  def indexToLinearIndex(idx: Index[_2D]) = idx(0) + idx(1) * size(0)
  def linearIndexToIndex(linearIdx: Int) = (Index(linearIdx % size(0), linearIdx / size(0)))

  val directions = Array(1.0, 0.0, 0.0, 1.0)

  override def isInside(pt: Point[_2D]): Boolean = {
    pt(0) >= origin(0) && pt(0) <= extent(0) &&
      pt(1) >= origin(1) && pt(1) <= extent(1)
  }


}

case class DiscreteImageDomain3D(val origin: Point[_3D], val spacing: Vector[_3D], val size: Index[_3D]) extends DiscreteImageDomain[_3D] {

  def points = for (k <- (0 until size(2)).view; j <- (0 until size(1)).view; i <- (0 until size(0)).view)
    yield Point(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j, origin(2) + spacing(2) * k)

  val extent : Point[_3D] = Point(origin(0) + spacing(0) * size(0), origin(1) + spacing(1) * size(1), origin(2) + spacing(2) * size(2)) // TODO replace with operator version
  def indexToLinearIndex(idx: Index[_3D]) = idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  def linearIndexToIndex(linearIdx: Int) =
    Index(
      linearIdx % (size(0) * size(1)) % size(0),
      linearIdx % (size(0) * size(1)) / size(0),
      linearIdx / (size(0) * size(1)))

  val directions = Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1)

  def isInside(pt: Point[_3D]): Boolean = {
    pt(0) >= origin(0) && pt(0) <= extent(0) &&
      pt(1) >= origin(1) && pt(1) <= extent(1) &&
      pt(2) >= origin(2) && pt(2) <= extent(2)
  }
  
}
