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
package scalismo.mesh

import java.io.File

import scalismo.ScalismoTestSuite
import scalismo.common.PointId
import scalismo.geometry.{ Point, Point3D, Vector, _3D }
import scalismo.io.MeshIO

class MeshMetricsTests extends ScalismoTestSuite {

  val path = getClass.getResource("/facemesh.stl").getPath
  val mesh = MeshIO.readMesh(new File(path)).get
  val translationLength = 1.0
  val translatedMesh = mesh.transform((pt: Point[_3D]) => pt + Vector(translationLength, 0.0, 0.0))

  describe("The ProcrustesDistanceMetric") {

    it("yields 0 between the same mesh") {
      MeshMetrics.procrustesDistance(mesh, mesh) should be(0.0 +- 1e-10)
    }

    it("should be 0 for a translated mesh") {
      MeshMetrics.procrustesDistance(mesh, translatedMesh) should be(0.0 +- 1e-5)
    }

    it("should be symmetric") {
      val deformedMesh = mesh.transform((pt: Point[_3D]) => Point3D(pt.x * 1.2, pt.y * 0.9, pt.z * 1.0))
      MeshMetrics.procrustesDistance(mesh, deformedMesh) should be(MeshMetrics.procrustesDistance(deformedMesh, mesh) +- 1e-5)
    }

  }

  describe("the average distance") {
    it("yields 0 between the same mesh") {
      MeshMetrics.avgDistance(mesh, mesh)
    }

    it("should be (slightly) lower than the average translation applied to each vertex") {
      MeshMetrics.avgDistance(mesh, translatedMesh) should be < translationLength.toDouble
      MeshMetrics.avgDistance(mesh, translatedMesh) should be(translationLength.toDouble +- (translationLength * 0.2))
    }
  }

  describe("The Hausdorff distance") {
    it("yields the value of the fixed translation transform") {
      MeshMetrics.hausdorffDistance(mesh, translatedMesh) should be(translationLength.toDouble +- 1e-5)
    }

    it("returns the max distance") {
      // create a mesh where the first vector is displaced by a value of 1
      val newMesh = mesh.transform((pt: Point[_3D]) => if (mesh.pointSet.findClosestPoint(pt).id == PointId(0)) pt + Vector(1, 0, 0) else pt)
      MeshMetrics.hausdorffDistance(mesh, newMesh) should be(1)
    }

    it("is symmetric") {
      MeshMetrics.hausdorffDistance(mesh, translatedMesh) should be(MeshMetrics.hausdorffDistance(translatedMesh, mesh))
    }
  }

  describe("the dice coefficient") {
    val path = getClass.getResource("/unit-sphere.stl").getPath
    val spheremesh = MeshIO.readMesh(new File(path)).get

    it("computes the right value for a unit sphere that completely overlaps itself") {
      MeshMetrics.diceCoefficient(spheremesh, spheremesh) should be(1)

    }

    it("computes the right value for a unit sphere that is shrunk by 0.5 ") {
      val spheremeshScaled = spheremesh.transform(pt => (pt.toVector * 0.5).toPoint)
      val smallSphereVolume = 0.5 * 0.5 * 0.5 * 4.0 / 3.0 * math.Pi
      val unitSphereVolume = 4.0 / 3.0 * math.Pi
      val intersectionVolume = smallSphereVolume
      val dc = 2.0 * intersectionVolume / (smallSphereVolume + unitSphereVolume)
      MeshMetrics.diceCoefficient(spheremesh, spheremeshScaled) should be(dc +- 1e-1)

    }

    it("yields 0 if the volumes don't overlap") {
      val spheremeshTranslated = spheremesh.transform(pt => pt + Vector(10, 0, 0))
      MeshMetrics.diceCoefficient(spheremesh, spheremeshTranslated) should be(0.0)
    }

  }

}

