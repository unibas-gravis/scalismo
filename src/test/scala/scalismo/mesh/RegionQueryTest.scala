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
import scalismo.common.{ BoxDomain, UnstructuredPointsDomain }
import scalismo.geometry._
import scalismo.image.{ DiscreteImageDomain }
import scalismo.io.MeshIO

class RegionQueryTest extends ScalismoTestSuite {

  val path = getClass.getResource("/facemesh.stl").getPath
  val mesh = MeshIO.readMesh(new File(path)).get
  val translationLength = 1.0
  val translatedMesh = mesh.transform((pt: Point[_3D]) => pt + Vector(translationLength, 0.0, 0.0))

  describe("The KD-tree region query") {

    it("finds points in the 2D bounding box region") {

      val img = DiscreteImageDomain(Point(0, 0), Vector(1, 1), IntVector(10, 10))
      val bigBox = img.boundingBox
      val dom = UnstructuredPointsDomain[_2D](img.points.toIndexedSeq)

      // Smaller Region
      val o = bigBox.origin
      val e = bigBox.extent * 0.5
      val nBox = BoxDomain(o, o + e)
      val pts = dom.findPointsInRegion(nBox).map(_.point)
      val groundTruth = dom.points.filter(p => nBox.isDefinedAt(p)).toSeq

      assert(groundTruth.forall(p => pts.contains(p)))
      assert(pts.forall(p => groundTruth.contains(p)))

    }

    it("finds points in the 3D bounding box region") {

      val bigBox = mesh.boundingBox

      //Smaller Region
      val o = bigBox.origin
      val e = bigBox.extent * 0.5
      val nBox = BoxDomain(o, o + e)
      val pts = mesh.pointSet.findPointsInRegion(nBox).map(_.point)
      val groundTruth = mesh.pointSet.points.filter(p => nBox.isDefinedAt(p)).toSeq

      assert(groundTruth.forall(p => pts.contains(p)))
      assert(pts.forall(p => groundTruth.contains(p)))

    }

  }
}
