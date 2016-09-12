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

import java.io.File

import scalismo.ScalismoTestSuite
import scalismo.geometry._
import scalismo.io.MeshIO
import scalismo.mesh.TriangleMesh
import scalismo.utils.{ Random, Memoize }

class SamplerTests extends ScalismoTestSuite {

  implicit val random: Random = Random(42)

  val facepath = getClass.getResource("/facemesh.stl").getPath
  val facemesh = MeshIO.readMesh(new File(facepath)).get

  describe("A uniform sampler") {
    it("yields approximately uniformly spaced points") {

      // precalculate values needed for determining if a point lies in a (triangle) cell.
      case class CellInfo(a: Point[_3D], b: Point[_3D], c: Point[_3D], v0: Vector[_3D], v1: Vector[_3D], dot00: Double, dot01: Double, dot11: Double)

      def infoForId(cellId: Int): CellInfo = {
        val cell = facemesh.cells(cellId)
        val vec = cell.pointIds.map(facemesh.pointSet.point)
        val (a, b, c) = (vec(0), vec(1), vec(2))
        val v0 = c - a
        val v1 = b - a

        // Compute dot products
        val dot00 = v0 dot v0
        val dot01 = v0 dot v1
        val dot11 = v1 dot v1

        CellInfo(a, b, c, v0, v1, dot00, dot01, dot11)
      }
      val memoizedInfo = Memoize(infoForId, facemesh.cells.size)

      def testSampling(numSamplingPoints: Int, randomAreaSizeRatio: Double): Unit = {

        // the algorithm is taken from http://www.blackpawn.com/texts/pointinpoly/
        def pointInCell(p: Point[_3D], cellId: Int, mesh: TriangleMesh[_3D]): Boolean = {
          val info = memoizedInfo(cellId)

          val v2 = p - info.a
          val dot02 = info.v0 dot v2
          val dot12 = info.v1 dot v2

          // Compute barycentric coordinates
          val invDenom = 1.0 / (info.dot00 * info.dot11 - info.dot01 * info.dot01)
          val u = (info.dot11 * dot02 - info.dot01 * dot12) * invDenom
          val v = (info.dot00 * dot12 - info.dot01 * dot02) * invDenom

          // Check if point is in triangle
          if ((u >= 0) && (v >= 0) && (u + v < 1)) {
            // this alone *should* be enough, but for some reason it isn't.
            // we additionally need to check if (A + u*v0 + v*v1) actually corresponds to
            // the point we were given.
            val check = info.a + info.v0 * u + info.v1 * v
            val diff = (p - check).norm

            //            if (diff < 0.01) true
            //            else {
            //                        println(s"$p is supposedly in ${(precalc.a, precalc.b, precalc.c)}")
            //                        println(s"u=$u v=$v")
            //                        println(s"difference: $diff")
            //            }

            diff < 0.01
          } else false
        }

        val sampler = UniformMeshSampler3D(facemesh, numSamplingPoints)
        val (samplePoints, _) = sampler.sample.unzip
        //        println(s"total number of points: ${facemesh.numberOfPoints}")

        def randomArea(mesh: TriangleMesh[_3D], targetRatio: Double): (IndexedSeq[Int], Double) = {
          val cellAreas = facemesh.cells.map(facemesh.computeTriangleArea)
          val cellIdWithArea = random.scalaRandom.shuffle(facemesh.cells.indices zip cellAreas)

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

        val numSampledPointsInArea = randomAreas.par.flatMap(cellId => {
          samplePoints.filter(sampledPoint => pointInCell(sampledPoint, cellId, facemesh))
        }).toSet.size

        val expectedNumberOfPointsInArea = actualRatio * numSamplingPoints
        //        println(s"expecting ~ ${expectedNumberOfPointsInArea.round} points to be found in randomly selected cells, actual found = $numSampledPointsInArea")
        //        println(f"ratio of points found vs expected = ${numSampledPointsInArea / expectedNumberOfPointsInArea}%1.3f")
        //        println()
        numSampledPointsInArea / expectedNumberOfPointsInArea should be(1.0 +- 0.1)

      }

      // for the full test: 1 to 3
      for (iRatio <- 1 to 1) {
        // full test: 1 to 3
        for (iPoints <- 1 to 1) {
          val points = iPoints * 5000
          val ratio = iRatio / 4.0
          testSampling(points, ratio)
        }
      }
    }
    it("yields different points when called multiple times") {
      val sampler = UniformMeshSampler3D(facemesh, 200)
      val pts1 = sampler.sample.map(_._1)
      val pts2 = sampler.sample.map(_._1)
      assert((pts1 diff pts2).nonEmpty)
    }
  }

  describe("A fixed point uniform mesh sampler") {
    it("yields the same points when called multiple times") {
      val sampler = FixedPointsUniformMeshSampler3D(facemesh, 200)
      val pts1 = sampler.sample.map(_._1)
      val pts2 = sampler.sample.map(_._1)
      assert((pts1 diff pts2).isEmpty)
    }
  }

}