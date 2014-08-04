package org.statismo.stk.core.numerics

import java.io.File

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry.{Vector3D, Point, ThreeD}
import org.statismo.stk.core.io.MeshIO
import org.statismo.stk.core.mesh.TriangleMesh

import scala.util.Random
import scala.collection.mutable

class SamplerTests extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

  val facepath = getClass.getResource("/facemesh.h5").getPath
  val facemesh = MeshIO.readMesh(new File(facepath)).get

  describe("A fixed point uniform sampler") {
    it("yields approximately uniformly spaced points") {

      val random = new Random()

      // precalculate values needed for determining if a point lies in a (triangle) cell.
      case class CellPrecalc(a: Vector3D, b: Vector3D, c: Vector3D, v0: Vector3D, v1: Vector3D, dot00: Float, dot01: Float, dot11: Float)

      val precalcCache = new mutable.HashMap[Int, CellPrecalc] {
        for (cellId <- 0 until facemesh.cells.size) {
          val cell = facemesh.cells(cellId)
          val vec = cell.pointIds.map(facemesh.points).map(_.toVector.asInstanceOf[Vector3D])
          val (a, b, c) = (vec(0), vec(1), vec(2))
          val v0 = c - a
          val v1 = b - a

          // Compute dot products
          val dot00 = v0 dot v0
          val dot01 = v0 dot v1
          val dot11 = v1 dot v1

          this(cellId) = CellPrecalc(a, b, c, v0, v1, dot00, dot01, dot11)
        }
      }

      def testSampling(numSamplingPoints: Int, randomAreaSizeRatio: Double): Unit = {

        def pointInCell(p: Point[ThreeD], cellId: Int, mesh: TriangleMesh): Boolean = {
          // the algorithm is taken from http://www.blackpawn.com/texts/pointinpoly/
          val precalc = precalcCache(cellId)

          val v2 = (p.toVector - precalc.a).asInstanceOf[Vector3D]
          val dot02 = precalc.v0 dot v2
          val dot12 = precalc.v1 dot v2

          // Compute barycentric coordinates
          val invDenom = 1.0 / (precalc.dot00 * precalc.dot11 - precalc.dot01 * precalc.dot01)
          val u = (precalc.dot11 * dot02 - precalc.dot01 * dot12) * invDenom
          val v = (precalc.dot00 * dot12 - precalc.dot01 * dot02) * invDenom

          // Check if point is in triangle
          if ((u >= 0) && (v >= 0) && (u + v < 1)) {
            // this alone *should* be enough, but for some reason it isn't.
            // we additionally need to check if (A + u*v0 + v*v1) actually corresponds to
            // the point we were given.
            val check = precalc.a + precalc.v0 * u + precalc.v1 * v
            val diff = (p - check).toVector.norm

            //            if (diff < 0.01) true
            //            else {
            //                        println(s"$p is supposedly in ${(precalc.a, precalc.b, precalc.c)}")
            //                        println(s"u=$u v=$v")
            //                        println(s"difference: $diff")
            //            }

            diff < 0.01
          } else false
        }


        val sampler = FixedPointsUniformMeshSampler3D(facemesh, numSamplingPoints, seed = random.nextInt())
        val (samplePoints, _) = sampler.sample.unzip
        //        println(s"total number of points: ${facemesh.numberOfPoints}")

        def randomArea(mesh: TriangleMesh, targetRatio: Double): (IndexedSeq[Int], Double) = {
          val cellAreas = facemesh.cells.map(facemesh.computeTriangleArea)
          val cellIdWithArea = random.shuffle((0 until facemesh.cells.size) zip cellAreas)

          var areaRemaining = mesh.area * targetRatio
          val areaCellIds = cellIdWithArea.takeWhile {
            case (id, area) =>
              areaRemaining -= area
              areaRemaining > 0
          }
          val (cellIds, _) = areaCellIds.unzip
          //          println(s"selected ${cellIds.size} out of ${facemesh.cells.size} cells")

          // areaRemaining is now the "overshoot" area. It is always <= 0.
          (cellIds, areaRemaining)
        }

        val (randomAreas, areaAdjust) = randomArea(facemesh, randomAreaSizeRatio)
        val actualRatio = (facemesh.area * randomAreaSizeRatio + areaAdjust) / facemesh.area
        //println(s"actual ratio of selected areas = $actualRatio")

        val randomAreasPar = randomAreas.par
        val samplePointsPar = samplePoints.par

        val numSampledPointsInArea = samplePointsPar.count(sampledPoint => {
          randomAreasPar.exists(cellId => pointInCell(sampledPoint, cellId, facemesh))
        })

        val expectedNumberOfPointsInArea = actualRatio * numSamplingPoints
        //        println(s"expecting ~ ${expectedNumberOfPointsInArea.round} points to be found in randomly selected cells, actual found = $numSampledPointsInArea")
        //        println(f"ratio of points found vs expected = ${numSampledPointsInArea / expectedNumberOfPointsInArea}%1.3f")
        //        println()
        numSampledPointsInArea / expectedNumberOfPointsInArea should be(1.0 plusOrMinus 0.1)

      }

      for (iRatio <- 1 to 3) {
        for (iPoints <- 1 to 3) {
          val points = iPoints * 5000
          val ratio = iRatio / 4.0
          testSampling(points, ratio)
        }
      }
    }
  }
}