package org.statismo.stk.core
package numerics

import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.common.BoxedDomain
import org.statismo.stk.core.image.DiscreteImageDomain
import org.statismo.stk.core.mesh.TriangleMesh
import breeze.stats.distributions.RandBasis
import org.statismo.stk.core.statisticalmodel.GaussianProcess
import org.apache.commons.math3.random.MersenneTwister
import java.util

/** sample generator typeclass */
trait Sampler[D <: Dim] {

  val numberOfPoints: Int
  /**
   * sample n points (x_1, ... x_n), yielding an sequence of (x_i, p(x_i)), i=1..n , p is the probbility density function
   * according to which the points are sampled
   */
  def sample: IndexedSeq[(Point[D], Double)]

  def volumeOfSampleRegion: Double
}


case class GridSampler[D <: Dim : NDSpace](domain: DiscreteImageDomain[D]) extends Sampler[D] {
  override def volumeOfSampleRegion = domain.volume
  override val numberOfPoints = domain.numberOfPoints

  val p = 1.0 / domain.volume


  override def sample = {
    domain.points.toIndexedSeq.map(pt => (pt, p))
  }
}

case class UniformSampler[D <: Dim : NDSpace](domain: BoxedDomain[D], numberOfPoints: Int) extends Sampler[D] {
  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume

  val ndSpace = implicitly[NDSpace[D]]

  val randGens = for (i <- (0 until ndSpace.dimensionality)) yield {
    breeze.stats.distributions.Uniform(domain.origin(i), domain.extent(i))
  }

  override def sample = {
    for (_ <- 0 until numberOfPoints) yield (Point.apply[D](randGens.map(r => r.draw().toFloat).toArray) ,p)
  }
}


case class SampleOnceSampler[D <: Dim](sampler: Sampler[D]) extends Sampler[D] {

  val numberOfPoints = sampler.numberOfPoints
  def volumeOfSampleRegion = sampler.volumeOfSampleRegion
  var sampleValues: IndexedSeq[(Point[D], Double)] = IndexedSeq()
  override def sample: IndexedSeq[(Point[D], Double)] = {

    if (sampleValues.size != 0) {
      sampleValues
    } else {
      sampleValues = sampler.sample
      sampleValues
    }
  }
}

case class RandomMeshSampler3D(mesh: TriangleMesh, numberOfPoints: Int, seed: Int) extends Sampler[_3D] {

  val p = 1.0 / mesh.area
  // should be replaced with real mesh volume
  val volumeOfSampleRegion = mesh.area
  def sample = {
    val points = mesh.points.toIndexedSeq
    val mt = new MersenneTwister()
    mt.setSeed(seed)
    val distrDim1 = breeze.stats.distributions.Uniform(0, mesh.numberOfPoints)(new RandBasis(mt))
    val pts = (0 until numberOfPoints).map(i => (points(distrDim1.draw().toInt), p))
    pts
  }
}

case class PointsWithLikelyCorrespondenceSampler(gp: GaussianProcess[_3D], refmesh: TriangleMesh, targetMesh: TriangleMesh, maxMd: Double) extends Sampler[_3D] {

  //  val meanPts = refmesh.points.map(gp.mean(_).toPoint)
  val meanPts = refmesh.points.map {
    x: Point[_3D] => x + gp.mean(x)
  }
  val ptsWithDist = refmesh.points.toIndexedSeq.zipWithIndex.par
    .map {
    case (refPt, refPtId) =>
      val (closestTgtPt, _) = targetMesh.findClosestPoint(meanPts.toIndexedSeq(refPtId))
      (refPt, gp.marginal(refPt).mahalanobisDistance(closestTgtPt - refPt))
  }

  val pts = ptsWithDist
    .filter {
    case (refPt, dist) => dist < maxMd
  }
    .map {
    case (refPt, dist) => (refPt, 1.0)
  }
    .map {
    case (refPt, dist) => (refPt, 1.0)
  }
    .toIndexedSeq

  override val volumeOfSampleRegion = 1.0
  override val numberOfPoints = pts.size
  override def sample = {
    println(s"Sampled: $numberOfPoints"); pts
  }
}

case class FixedPointsUniformMeshSampler3D(mesh: TriangleMesh, numberOfPoints: Int, seed: Int) extends Sampler[_3D] {

  override val volumeOfSampleRegion = mesh.area

  private val p = 1.0 / mesh.area

  val samplePoints = {

    val accumulatedAreas: Array[Double] = mesh.cells.scanLeft(0.0) {
      case (sum, cell) =>
        sum + mesh.computeTriangleArea(cell)
    }.tail.toArray

    val random = new scala.util.Random(seed)

    for (i <- 0 until numberOfPoints) yield {
      val drawnValue = random.nextDouble() * mesh.area

      val indexOrInsertionPoint = util.Arrays.binarySearch(accumulatedAreas, drawnValue)
      val index = if (indexOrInsertionPoint >= 0) indexOrInsertionPoint else -(indexOrInsertionPoint + 1)
      assert(index >= 0 && index < accumulatedAreas.length)

      mesh.samplePointInTriangleCell(mesh.cells(index), random.nextInt())
    }
  }

  override def sample = {
    samplePoints.map(pt => (pt, p))
  }
}

case class FixedPointsMeshSampler3D(mesh: TriangleMesh, numberOfPoints: Int, seed: Int) extends Sampler[_3D] {

  val volumeOfSampleRegion = mesh.area
  val p = 1.0 / mesh.area

  scala.util.Random.setSeed(seed)
  val meshPoints = mesh.points.toIndexedSeq
  val samplePoints = for (i <- 0 until numberOfPoints) yield {
    val idx = scala.util.Random.nextInt(mesh.numberOfPoints)
    meshPoints(idx)
  }
  assert(samplePoints.size == numberOfPoints)

  def sample = {
    samplePoints.map(pt => (pt, p))
  }
}


//}
