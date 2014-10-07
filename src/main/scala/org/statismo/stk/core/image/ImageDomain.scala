package org.statismo.stk.core
package image

import common.{ DiscreteDomain, BoxedDomain }
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.FiniteDiscreteDomain
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import breeze.linalg.DenseVector

abstract class DiscreteImageDomain[D <: Dim: DimOps] extends FiniteDiscreteDomain[D] with BoxedDomain[D] {

  val origin: Point[D]
  val spacing: Vector[D]
  val size: Index[D]

  override def extent: Point[D] = origin + Vector((spacing.toBreezeVector :* size.toBreezeVector.map(_.toFloat)).data)

  def directions: Array[Double] = (0 until (size.dimensionality * size.dimensionality)).map(i => if (i % size.dimensionality == i / size.dimensionality) 1.0 else 0.0) toArray

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  override def points: Iterator[Point[D]]

  def indexToLinearIndex(idx: Index[D]): Int

  def linearIndexToIndex(linearIdx: Int): Index[D]

}

private case class DiscreteImageDomain1D(origin: Point[_1D], spacing: Vector[_1D], size: Index[_1D]) extends DiscreteImageDomain[_1D] {
  private val _points = for (i <- (0 until size(0))) yield Point(origin(0) + spacing(0) * i)
  override def points: Iterator[Point[_1D]] = _points.toIterator
  def indexToLinearIndex(idx: Index[_1D]) = idx(0)
  def linearIndexToIndex(linearIdx: Int) = Index[_1D](Array(linearIdx))
}

private case class DiscreteImageDomain2D(origin: Point[_2D], spacing: Vector[_2D], size: Index[_2D]) extends DiscreteImageDomain[_2D] {
  private val _points = for (j <- (0 until size(1)); i <- (0 until size(0))) yield Point(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j)
  override def points: Iterator[Point[_2D]] = _points.toIterator
  def indexToLinearIndex(idx: Index[_2D]) = idx(0) + idx(1) * size(0)
  def linearIndexToIndex(linearIdx: Int) = Index[_2D](Array(linearIdx % size(0), linearIdx / size(0)))
}

private case class DiscreteImageDomain3D(origin: Point[_3D], spacing: Vector[_3D], size: Index[_3D]) extends DiscreteImageDomain[_3D] {
  private val _points = for (k <- (0 until size(2)); j <- (0 until size(1)); i <- (0 until size(0)))
    yield Point(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j, origin(2) + spacing(2) * k)
  override def points: Iterator[Point[_3D]] = _points.toIterator
  def indexToLinearIndex(idx: Index[_3D]) = idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
  def linearIndexToIndex(linearIdx: Int) = Index[_3D](Array(
    linearIdx % (size(0) * size(1)) % size(0),
    linearIdx % (size(0) * size(1)) / size(0),
    linearIdx / (size(0) * size(1))))
}

object DiscreteImageDomain {

  trait Create[D <: Dim] {
    def createImageDomain(origin: Point[D], spacing: Vector[D], size: Index[D]): DiscreteImageDomain[D]
  }

  implicit object createImageDomain2D extends Create[_2D] {
    override def createImageDomain(origin: Point[_2D], spacing: Vector[_2D], size: Index[_2D]): DiscreteImageDomain[_2D] = new DiscreteImageDomain2D(origin, spacing, size)
  }
  
  implicit object createImageDomain3D extends Create[_3D] {
    override def createImageDomain(origin: Point[_3D], spacing: Vector[_3D], size: Index[_3D]): DiscreteImageDomain[_3D] = new DiscreteImageDomain3D(origin, spacing, size)
  } 
  implicit object createImageDomain1D extends Create[_1D] {
    override def createImageDomain(origin: Point[_1D], spacing: Vector[_1D], size: Index[_1D]): DiscreteImageDomain[_1D] = new DiscreteImageDomain1D(origin, spacing, size)
  }

  def apply[D <: Dim](origin: Point[D], spacing: Vector[D], size: Index[D])(implicit evDim: DimOps[D], evCreateRot: Create[D]) = {
    evCreateRot.createImageDomain(origin, spacing, size)
  }

}


