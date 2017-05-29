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
import scalismo.geometry.Point

class UnstructuredPointsDomainTests extends ScalismoTestSuite {

  describe("An UnstructuredPointsDomain[_3D]") {
    // point sequence, note double occurrence of last point
    val points = IndexedSeq(Point(0.23, 189.0, 23), Point(2.3, -189.0, 45), Point(-32, 8.0, -56), Point(-32, 8.0, -56))
    val dom = UnstructuredPointsDomain(points)

    it("is equal if constructed from same point set") {
      UnstructuredPointsDomain(points) shouldBe dom
    }

    it("has a consistent hash value") {
      dom.hashCode() shouldBe dom.hashCode()
      UnstructuredPointsDomain(points).hashCode() shouldBe dom.hashCode()
    }

    it("has the same hash value as the underlying point sequence") {
      dom.hashCode() shouldBe points.hashCode()
    }
  }
}
