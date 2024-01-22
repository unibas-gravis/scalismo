/*
 * Copyright University of Basel, Graphics and Vision Research Group
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

package scalismo.common

import scalismo.ScalismoTestSuite
import scalismo.geometry.{_2D, _3D, Point, Point2D, Point3D}

class PointSetTest extends ScalismoTestSuite {
  describe("A pointset") {
    // point sequence, note double occurrence of last point
    val pointSet: PointSet[_2D] = UnstructuredPoints2D(
      IndexedSeq(
        Point2D(2, 1),
        Point2D(2, 2),
        Point2D(3, 1.5)
      )
    )

    it("reports the correct center of mass") {
      (pointSet.centerOfMass - Point2D(7 / 3.0, 4.5 / 3.0)).norm should be < 1e-5
    }
  }
}
