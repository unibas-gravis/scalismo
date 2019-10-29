/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.numerics

import java.util

import breeze.stats.distributions.Uniform
import scalismo.common.BoxDomain
import scalismo.geometry._
import scalismo.image.DiscreteImageDomain
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.GaussianProcess
import scalismo.tetramesh.TetrahedralMesh
import scalismo.utils.Random

/** sample generator typeclass */
trait Sampler[D] {

  val numberOfPoints: Int
  /**
   * sample n points (x_1, ... x_n), yielding an sequence of (x_i, p(x_i)), i=1..n , p is the probability density function
   * according to which the points are sampled
   */
  def sample(): IndexedSeq[(Point[D], Double)]

  def volumeOfSampleRegion: Double
}

case class GridSampler[D: NDSpace](domain: DiscreteImageDomain[D]) extends Sampler[D] {
  override def volumeOfSampleRegion = domain.boundingBox.volume
  override val numberOfPoints = domain.numberOfPoints

  val p = 1.0 / volumeOfSampleRegion
  override def sample() = {
    domain.points.toIndexedSeq.map(pt => (pt, p))
  }
}

case class UniformSampler[D: NDSpace](domain: BoxDomain[D], numberOfPoints: Int)(implicit rand: Random) extends Sampler[D] {

  def volumeOfSampleRegion = domain.volume
  val p = 1.0 / domain.volume

  override def sample() = {
    val ndSpace = implicitly[NDSpace[D]]
    val randGens = for (i <- (0 until ndSpace.dimensionality)) yield {
      Uniform(domain.origin(i), domain.oppositeCorner(i))(rand.breezeRandBasis)
    }

    for (_ <- 0 until numberOfPoints) yield (Point.apply[D](randGens.map(r => r.draw()).toArray), p)
  }
}

case class RandomMeshSampler3D(mesh: TriangleMesh[_3D], numberOfPoints: Int, seed: Int)(implicit rand: Random) extends Sampler[_3D] {

  val p = 1.0 / mesh.area

  // should be replaced with real mesh volume
  val volumeOfSampleRegion = mesh.area
  def sample() = {
    val points = mesh.pointSet.points.toIndexedSeq
    val distrDim1 = Uniform(0, mesh.pointSet.numberOfPoints)(rand.breezeRandBasis)
    val pts = (0 until numberOfPoints).map(i => (points(distrDim1.draw().toInt), p))
    pts
  }
}

case class RandomMeshVolumeSampler3D(mesh: TetrahedralMesh[_3D], numberOfPoints: Int, seed: Int)(implicit rand: Random) extends Sampler[_3D] {

  val p = 1.0 / mesh.volume

  // should be replaced with real mesh volume
  val volumeOfSampleRegion = mesh.volume
  def sample() = {
    val points = mesh.pointSet.points.toIndexedSeq
    val distrDim1 = Uniform(0, mesh.pointSet.numberOfPoints)(rand.breezeRandBasis)
    val pts = (0 until numberOfPoints).map(i => (points(distrDim1.draw().toInt), p))
    pts
  }
}

case class PointsWithLikelyCorrespondenceSampler(gp: GaussianProcess[_3D, EuclideanVector[_3D]], refmesh: TriangleMesh[_3D], targetMesh: TriangleMesh[_3D], maxMd: Double) extends Sampler[_3D] {

  //  val meanPts = refmesh.points.map(gp.mean(_).toPoint)
  val meanPts = refmesh.pointSet.points.map {
    x: Point[_3D] => x + gp.mean(x)
  }
  val ptsWithDist = refmesh.pointSet.points.toIndexedSeq.zipWithIndex.par
    .map {
      case (refPt, refPtId) =>
        val closestTgtPt = targetMesh.pointSet.findClosestPoint(meanPts.toIndexedSeq(refPtId)).point
        (refPt, gp.marginal(refPt).mahalanobisDistance((closestTgtPt - refPt).toBreezeVector))
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
  override def sample() = {
    println(s"Sampled: $numberOfPoints"); pts
  }
}


case class PointsWithLikelyCorrespondenceMeshVolumeSampler(gp: GaussianProcess[_3D, EuclideanVector[_3D]], refmesh: TetrahedralMesh[_3D], targetMesh: TetrahedralMesh[_3D], maxMd: Double) extends Sampler[_3D] {

  //  val meanPts = refmesh.points.map(gp.mean(_).toPoint)
  val meanPts = refmesh.pointSet.points.map {
    x: Point[_3D] => x + gp.mean(x)
  }
  val ptsWithDist = refmesh.pointSet.points.toIndexedSeq.zipWithIndex.par
    .map {
      case (refPt, refPtId) =>
        val closestTgtPt = targetMesh.pointSet.findClosestPoint(meanPts.toIndexedSeq(refPtId)).point
        (refPt, gp.marginal(refPt).mahalanobisDistance((closestTgtPt - refPt).toBreezeVector))
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
  override def sample() = {
    println(s"Sampled: $numberOfPoints"); pts
  }
}






case class UniformMeshVolumeSampler3D(mesh: TetrahedralMesh[_3D], numberOfPoints: Int)(implicit rand: Random) extends Sampler[_3D] {

  override val volumeOfSampleRegion: Double = mesh.volume

  private val p: Double = 1.0 / mesh.volume

  val accumulatedAreas: Array[Double] = mesh.cells.scanLeft(0.0) {
    case (sum, cell) =>
      sum + mesh.computeTrahedronVolume(cell)
  }.tail.toArray

  override def sample() = {
    val samplePoints = {
      for (i <- 0 until numberOfPoints) yield {
        val drawnValue = rand.scalaRandom.nextDouble() * mesh.volume
        val indexOrInsertionPoint = util.Arrays.binarySearch(accumulatedAreas, drawnValue)
        val index = if (indexOrInsertionPoint >= 0) indexOrInsertionPoint else -(indexOrInsertionPoint + 1)
        assert(index >= 0 && index < accumulatedAreas.length)
        mesh.samplePointInTetrahedralCell(mesh.cells(index))
      }
    }

    samplePoints.map(pt => (pt, p))
  }
}

case class UniformMeshSampler3D(mesh: TriangleMesh[_3D], numberOfPoints: Int)(implicit rand: Random) extends Sampler[_3D] {

  override val volumeOfSampleRegion: Double = mesh.area

  private val p: Double = 1.0 / mesh.area

  val accumulatedAreas: Array[Double] = mesh.cells.scanLeft(0.0) {
    case (sum, cell) =>
      sum + mesh.computeTriangleArea(cell)
  }.tail.toArray

  override def sample() = {
    val samplePoints = {
      for (i <- 0 until numberOfPoints) yield {
        val drawnValue = rand.scalaRandom.nextDouble() * mesh.area
        val indexOrInsertionPoint = util.Arrays.binarySearch(accumulatedAreas, drawnValue)
        val index = if (indexOrInsertionPoint >= 0) indexOrInsertionPoint else -(indexOrInsertionPoint + 1)
        assert(index >= 0 && index < accumulatedAreas.length)
        mesh.samplePointInTriangleCell(mesh.cells(index))
      }
    }

    samplePoints.map(pt => (pt, p))
  }
}



case class FixedPointsUniformMeshSampler3D(mesh: TriangleMesh[_3D], numberOfPoints: Int)(implicit rng: Random) extends Sampler[_3D] {
  override val volumeOfSampleRegion = mesh.area
  val samplePoints = UniformMeshSampler3D(mesh, numberOfPoints).sample()
  override def sample() = samplePoints
}


case class FixedPointsUniformMeshVolumeSampler3D(mesh: TetrahedralMesh[_3D], numberOfPoints: Int)(implicit rng: Random) extends Sampler[_3D] {
  override val volumeOfSampleRegion = mesh.volume
  val samplePoints = UniformMeshVolumeSampler3D(mesh, numberOfPoints).sample()
  override def sample() = samplePoints
}

case class FixedPointsMeshSampler3D(mesh: TriangleMesh[_3D], numberOfPoints: Int)(implicit rand: Random) extends Sampler[_3D] {

  val volumeOfSampleRegion = mesh.area
  val p = 1.0 / mesh.area

  val meshPoints = mesh.pointSet.points.toIndexedSeq

  def samplePoints = for (i <- 0 until numberOfPoints) yield {
    val idx = rand.scalaRandom.nextInt(mesh.pointSet.numberOfPoints)
    meshPoints(idx)
  }

  var lastSampledPoints: Option[IndexedSeq[(Point[_3D], Double)]] = None

  def sample() = {

    lastSampledPoints match {
      case Some(lastSampledPoints) => lastSampledPoints
      case None => {
        val pts = samplePoints.map(pt => (pt, p))
        lastSampledPoints = Some(pts)
        pts
      }

    }
  }
}


case class FixedPointsMeshVolumeSampler3D(mesh: TetrahedralMesh[_3D], numberOfPoints: Int)(implicit rand: Random) extends Sampler[_3D] {

  val volumeOfSampleRegion = mesh.volume
  val p = 1.0 / mesh.volume

  val meshPoints = mesh.pointSet.points.toIndexedSeq

  def samplePoints = for (i <- 0 until numberOfPoints) yield {
    val idx = rand.scalaRandom.nextInt(mesh.pointSet.numberOfPoints)
    meshPoints(idx)
  }

  var lastSampledPoints: Option[IndexedSeq[(Point[_3D], Double)]] = None

  def sample() = {

    lastSampledPoints match {
      case Some(lastSampledPoints) => lastSampledPoints
      case None => {
        val pts = samplePoints.map(pt => (pt, p))
        lastSampledPoints = Some(pts)
        pts
      }

    }
  }
}

//}
