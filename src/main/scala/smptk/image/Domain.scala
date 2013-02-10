package smptk.image

import scala.language.higherKinds
import scala.language.implicitConversions

import breeze.linalg.DenseVector
import smptk.image.Geometry._

trait Domain[CV[A] <: CoordVector[A]] {
  def dimensionality: Int
}

trait ContinuousDomain[CV[A] <: CoordVector[A]] extends Domain[CV] {
}

trait ContinuousImageDomain[CV[A] <: CoordVector[A]] extends ContinuousDomain[CV] {
  //def origin: CV[Float]
  //def extent: CV[Float]
  //def directions: CV[CV[Float]]
  val isInside : CV[Float] => Boolean
}

case class ContinuousImageDomain1D(val isInside: Point1D => Boolean) extends ContinuousImageDomain[CoordVector1D] {
  def dimensionality = 1

}

case class ContinuousImageDomain2D(val isInside: Point2D => Boolean) extends ContinuousImageDomain[CoordVector2D] {
  def dimensionality = 2
}

case class ContinuousImageDomain3D(val isInside: Point3D => Boolean) extends ContinuousImageDomain[CoordVector3D] {
  def dimensionality = 3
}

trait DiscreteDomain[CV[A] <: CoordVector[A]] extends Domain[CV] {
  def points: IndexedSeq[CV[Float]]

  def numberOfPoints: Int

  //def neighbors(pt: CoordVector): IndexedSeq[CoordVector]
  def isDefinedAt(i: Int) = i >= 0 && i <= points.size
}

trait DiscreteImageDomain[CV[A] <: CoordVector[A]] extends DiscreteDomain[CV] { //extends ImageDomain[Point] {
  def origin: CV[Float]
  def spacing: CV[Float]
  def size: CV[Int]
  def extent: CV[Float]
  def directions: Array[Double]

  def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  def indexToLinearIndex(idx: CV[Int]): Int
  def linearIndexToIndex(linearIdx: Int): CV[Int]

  def isInside(pt: CV[Float]): Boolean

}

case class DiscreteImageDomain1D(val origin: CoordVector1D[Float], val spacing: CoordVector1D[Float], val size: CoordVector1D[Int]) extends DiscreteImageDomain[CoordVector1D] {
  val dimensionality = 1
  val points = for (i <- 0 until size(0)) yield CoordVector1D(origin(0) + spacing(0) * i)

  val extent = CoordVector1D(origin(0) + spacing(0) * size(0))

  def indexToLinearIndex(idx: CoordVector1D[Int]) = idx(0)
  def linearIndexToIndex(linearIdx: Int) = linearIdx

  val directions = Array(1.)

  def isInside(pt: Point1D): Boolean = {
    pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0)
  }

}

case class DiscreteImageDomain2D(val origin: CoordVector2D[Float], val spacing: CoordVector2D[Float], val size: CoordVector2D[Int]) extends DiscreteImageDomain[CoordVector2D] {
  val dimensionality = 2
  val points = for (j <- 0 until size(1); i <- 0 until size(0))
      yield CoordVector2D(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j)
  

  val extent = CoordVector2D(origin(0) + spacing(0) * size(0), origin(1) + spacing(1) * size(1))

  def indexToLinearIndex(idx: CoordVector2D[Int]) = idx(0) + idx(1) * size(0)
  def linearIndexToIndex(linearIdx: Int) = (linearIdx % size(0), linearIdx / size(0))

  val directions = Array(1., 0., 0., 1.)

  def isInside(pt: Point2D): Boolean = {
    pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0) &&
      pt(1) >= origin(1) && pt(1) <= origin(1) + extent(1)
  }

}

case class DiscreteImageDomain3D(val origin: CoordVector3D[Float], val spacing: CoordVector3D[Float], val size: CoordVector3D[Int]) extends DiscreteImageDomain[CoordVector3D] {
  val dimensionality = 3
  val points  = for (k <- 0 until size(2); j <- 0 until size(1); i <- 0 until size(0))
      yield CoordVector3D(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j, origin(2) + spacing(2) * k)


  val extent = CoordVector3D(origin(0) + spacing(0) * size(0), origin(1) + spacing(1) * size(1), origin(2) + spacing(2) * size(2))
  def indexToLinearIndex(idx: CoordVector3D[Int]) = idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  def linearIndexToIndex(linearIdx: Int) =
    (
      linearIdx % (size(0) * size(1)) % size(0),
      linearIdx % (size(0) * size(1)) / size(0),
      linearIdx / (size(0) * size(1)))

  val	 directions = Array(1., 0., 0., 0., 1., 0., 0., 0., 1)

  def isInside(pt: Point3D): Boolean = {
    pt(0) >= origin(0) && pt(0) <= origin(0) + extent(0) &&
      pt(1) >= origin(1) && pt(1) <= origin(1) + extent(1) &&
      pt(2) >= origin(2) && pt(2) <= origin(2) + extent(2)
  }

}
