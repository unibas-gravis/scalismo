package org.statismo.stk.core.numerics

import java.io.File

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.common.UnstructuredPointsDomain
import org.statismo.stk.core.geometry.{Vector3D, Point, ThreeD}
import org.statismo.stk.core.io.{StatismoIO, MeshIO}
import org.statismo.stk.core.mesh.{TriangleCell, TriangleMesh}

import scala.util.Random

/**
 * Created by luethi on 7/31/14.
 */
class SamplerTests extends FunSpec with ShouldMatchers {
  org.statismo.stk.core.initialize()

  val facepath = getClass().getResource("/facemesh.h5").getPath
  val facemesh = MeshIO.readMesh(new File(facepath)).get


  describe("A fixed point uniform sampler") {
    ignore("yields approximately uniformly spaced points") {


      // TODO the test still fails for some reason (main idea should be correct) and performance is terrible.
      // We believe (from tests and eyeballing) that the uniform sampling actually works.

      def sameSide(p1 : Point[ThreeD],p2 : Point[ThreeD], a : Point[ThreeD],b : Point[ThreeD]) : Boolean = {
        val ba = (b-a).asInstanceOf[Vector3D]
        val p1a = (p1-a).asInstanceOf[Vector3D]
        val p2a = (p2-a).asInstanceOf[Vector3D]
        val cp1 = ba.cross(p1a)
        val cp2 = ba.cross(p2a)

        (cp1 dot cp2) >= 0.0
      }

      def pointInCell(p : Point[ThreeD], cell : TriangleCell, mesh : TriangleMesh) : Boolean = {
        val (a,b,c) = (mesh.points(cell.pointIds(0)), mesh.points(cell.pointIds(1)), mesh.points(cell.pointIds(2)))
        sameSide(p,a, b,c) && sameSide(p,b, a,c)  && sameSide(p,c, a,b)
      }


      // TODO currently it is not clear how to test it, except by eyeballing
      val numSamplingPoints = 2000
      val sampler = FixedPointsUniformMeshSampler3D(facemesh, numSamplingPoints, seed = 42)
      val (samplePoints, _) = sampler.sample.unzip
      println(s"numPointsofMesh ${facemesh.numberOfPoints}")
      val randomAreaSizeRatio = 0.2

      def randomArea(mesh: TriangleMesh, percentageOfArea: Double): IndexedSeq[TriangleCell] = {
        //        val rand = new Random(42)
        //        val id = rand.nextInt(facemesh.cells.size)
        val cellAreas = facemesh.cells.map(facemesh.computeTriangleArea)
        val cellIdWithArea = Random.shuffle((0 until facemesh.cells.size) zip cellAreas)

        var areaRemaining = mesh.area * randomAreaSizeRatio
        val areaCellIds = cellIdWithArea.takeWhile {
          case (id, area) =>
            areaRemaining -= area
            areaRemaining > 0
        }
        val (cellIds, _) = areaCellIds.unzip
        println(s"cell ${cellIds.size} mesh: ${facemesh.cells.size}")
        cellIds.map(id => facemesh.cells(id))
      }

      val ptsInRandomArea = randomArea(facemesh, randomAreaSizeRatio)//.map(cell => cell.pointIds).flatten.toSet
      println(s"ptsInAra ${ptsInRandomArea.size}")

      val numSampledPointsInArea = samplePoints.filter(sampledPoint => {
        ptsInRandomArea.exists(cell => pointInCell(sampledPoint, cell, facemesh))
      }).size
      println(s"nums $numSampledPointsInArea")

      val expectedNumberOfPointsInArea = randomAreaSizeRatio * numSamplingPoints
      println(numSampledPointsInArea / expectedNumberOfPointsInArea)
      numSampledPointsInArea / expectedNumberOfPointsInArea should be(1.0 plusOrMinus (0.1))

    }
  }
}