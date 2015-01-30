package org.statismo.stk.core.numerics

import org.statismo.stk.core.image._
import org.statismo.stk.core.common.VectorField
import breeze.linalg.DenseVector
import org.statismo.stk.core.geometry._


case class Integrator[D <: Dim: NDSpace](sampler: Sampler[D]) {

  def integrateScalar(img: ScalarImage[D]): Float = {
    integrateScalar(img.liftValues)
  }

  def integrateScalar(f: Function1[Point[D], Option[Float]]): Float = {
    val samples = sampler.sample

    val sum = samples.par.map { case (pt, p) => f(pt).getOrElse(0f) * (1f / p.toFloat)}.sum

    sum / samples.size
  }

  def integrateVector[DO <: Dim : NDSpace](img: VectorField[D, DO]): Vector[DO] = {
    integrateVector(img.liftValues)
  }

  def integrateVector[DO <: Dim : NDSpace](f: Function1[Point[D], Option[Vector[DO]]]): Vector[DO] = {
    val samples = sampler.sample

    val zeroVector = Vector.zeros[DO]
    val sum = samples.par.map { case (pt, p) => f(pt).getOrElse(zeroVector) * (1f / p.toFloat) }.foldLeft(zeroVector)((a, b) => { a + b })
    sum * (1f / (sampler.numberOfPoints - 1).toFloat)
  }

  def integrateVector(f: Function1[Point[D], Option[DenseVector[Float]]], dimensionality: Int): DenseVector[Float] = {
    val samples = sampler.sample

    val zeroVector = DenseVector.zeros[Float](dimensionality)
    val sum = samples.par.map { case (pt, p) => f(pt).getOrElse(zeroVector) * (1f / p.toFloat) }.foldLeft(zeroVector)((a, b) => { a + b })
    sum * (1f / (sampler.numberOfPoints - 1).toFloat)
  }

}


