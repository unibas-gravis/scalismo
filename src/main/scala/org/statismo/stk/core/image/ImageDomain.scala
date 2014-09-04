package org.statismo.stk.core
package image

import common.{ DiscreteDomain, BoxedDomain }
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.FiniteDiscreteDomain
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import breeze.linalg.DenseVector

case class DiscreteImageDomain[D <: Dim: DimOps](origin: Point[D], spacing: Vector[D], size: Index[D]) extends FiniteDiscreteDomain[D] with BoxedDomain[D] {

  override def extent: Point[D] = origin + Vector((spacing.toBreezeVector :* size.toBreezeVector.map(_.toFloat)).data)

  def directions: Array[Double] = (0 until (size.dimensionality * size.dimensionality)).map(i => if (i % size.dimensionality == i / size.dimensionality) 1.0 else 0.0) toArray

  override def numberOfPoints = (0 until size.dimensionality).foldLeft(1)((res, d) => res * size(d))

  private val _points = size.dimensionality match {
    case 1 => for (i <- (0 until size(0))) yield Point(origin(0) + spacing(0) * i)
    case 2 => for (j <- (0 until size(1)); i <- (0 until size(0)))
      yield Point(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j)
    case 3 => for (k <- (0 until size(2)); j <- (0 until size(1)); i <- (0 until size(0)))
      yield Point(origin(0) + spacing(0) * i, origin(1) + spacing(1) * j, origin(2) + spacing(2) * k)
    case _ => throw new NotImplementedError("Only dimensionality 1, 2 and 3 are supported for DiscreteImageDomain")
  }

  override def points = _points.toIterator.asInstanceOf[Iterator[Point[D]]]
  

  def indexToLinearIndex(idx: Index[D]): Int = idx.dimensionality match {
    case 1 => idx(0)
    case 2 => idx(0) + idx(1) * size(0)
    case 3 => idx(0) + idx(1) * size(0) + idx(2) * size(0) * size(1)
    case _ => throw new NotImplementedError("Only dimensionality 1, 2 and 3 are supported for DiscreteImageDomain")

  }
  def linearIndexToIndex(linearIdx: Int): Index[D] = {
    val t = size.dimensionality match {

      case 1 => Index[_1D](Array(linearIdx))
      case 2 => Index[_2D](Array(linearIdx % size(0), linearIdx / size(0)))
      case 3 => Index[_3D](Array(
        linearIdx % (size(0) * size(1)) % size(0),
        linearIdx % (size(0) * size(1)) / size(0),
        linearIdx / (size(0) * size(1))))
      case _ => throw new NotImplementedError("Only dimensionality 1, 2 and 3 are supported for DiscreteImageDomain")
    }
    t.asInstanceOf[Index[D]]
  }
}



