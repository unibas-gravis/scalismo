package scalismo.mesh

import java.io.File

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.ShouldMatchers
import scalismo.geometry.{_3D, Point, Vector}
import scalismo.io.MeshIO


class MeshMetricsTests extends FunSpec with ShouldMatchers {

  scalismo.initialize()

  val path = getClass().getResource("/facemesh.h5").getPath
  val mesh = MeshIO.readMesh(new File(path)).get
  val translationLength = 1.0f
  val translatedMesh = mesh.transform((pt: Point[_3D]) => pt + Vector(translationLength, 0.0f, 0.0f))
  
  describe("The ProcrustesDistanceMetric") {

    it("yields 0 between the same mesh") {
      MeshMetrics.procrustesDistance(mesh, mesh) should be(0.0)
    }

    it("should be 0 for a translated mesh") {
      MeshMetrics.procrustesDistance(mesh, translatedMesh) should be(0.0 plusOrMinus 1e-5)
    }

  }

  describe("the average distance") {
    it("yields 0 between the same mesh") {
      MeshMetrics.avgDistance(mesh, mesh)
    }
    
    it("should be (slightly) lower than the average translation applied to each vertex") {
      MeshMetrics.avgDistance(mesh, translatedMesh) should be < (translationLength.toDouble)
      MeshMetrics.avgDistance(mesh, translatedMesh) should be(translationLength.toDouble plusOrMinus(translationLength * 0.2))
    }
  }

  describe("The Haussdorf distance") {
    it("yields the value of the fixed translation transform") {
      MeshMetrics.hausdorffDistance(mesh, translatedMesh) should be(translationLength.toDouble plusOrMinus 1e-5)
    } 
    
    it ("returns the max distance") {
      // create a mesh where the first vector is displaced by a value of 1
      val newMesh = mesh.transform((pt : Point[_3D]) => if(mesh.findClosestPoint(pt)._2 == 0) pt + Vector(1,0,0) else pt)
      MeshMetrics.hausdorffDistance(mesh, newMesh) should be(1)
    }
    
    it("is symmetric") {
      MeshMetrics.hausdorffDistance(mesh, translatedMesh) should be(MeshMetrics.hausdorffDistance(translatedMesh, mesh))
    }
  }
  
  describe("the dice coefficient") {
      val path = getClass().getResource("/unit-sphere.vtk").getPath
      val spheremesh = MeshIO.readMesh(new File(path)).get

    it("computes the right value for an unit sphere that compeltely overlaps itself") {
      MeshMetrics.diceCoefficient(spheremesh, spheremesh) should be(1)
               
    }

    it("computes the right value for an unit sphere that is shrunk by 0.5 ") {
      val spheremeshScaled = spheremesh.transform(pt => (pt.toVector * 0.5).toPoint)
      val smallSphereVolume = 0.5 * 0.5 * 0.5 * 4.0 / 3.0 * math.Pi
      val unitSphereVolume =  4.0/ 3.0 * math.Pi
      val intersectionVolume = smallSphereVolume
      val dc = 2.0 * intersectionVolume / (smallSphereVolume + unitSphereVolume)
      MeshMetrics.diceCoefficient(spheremesh, spheremeshScaled) should be(dc plusOrMinus 1e-1)
               
    }

    it("yields 0 if the volumes don't overlap") { 
      val spheremeshTranslated = spheremesh.transform(pt => pt + Vector(10, 0, 0))
      MeshMetrics.diceCoefficient(spheremesh, spheremeshTranslated) should be(0.0)
    }

  }
  
}


