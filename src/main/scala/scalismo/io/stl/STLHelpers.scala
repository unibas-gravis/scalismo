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
package scalismo.io.stl

import scalismo.common.PointId
import scalismo.geometry.{EuclideanVector3D, Point3D}
import scalismo.mesh.{TriangleCell, TriangleList, TriangleMesh3D}

import java.nio.ByteOrder
import scala.collection.mutable

private val ORDER = ByteOrder.LITTLE_ENDIAN
private val HEADER_LENGTH = 80

case class STLTriangle(n: EuclideanVector3D, p1: Point3D, p2: Point3D, p3: Point3D)

package object STLHelpers {
  def STLTrianglesToTriangleMesh(stlTriangles: Seq[STLTriangle]): TriangleMesh3D = {
    val pointsMap = mutable.HashMap.empty[Point3D, Int]
    val pointsArray = new Array[Point3D](stlTriangles.length * 3)
    var pointCounter = 0

    def getOrCreatePointId(point: Point3D): Int =
      pointsMap.getOrElseUpdate(point, {
                                  val id = pointCounter
                                  pointsArray(id) = point
                                  pointCounter += 1
                                  id
                                }
      )

    val triangles = stlTriangles.map { triangle =>
      val ptId1 = getOrCreatePointId(triangle.p1)
      val ptId2 = getOrCreatePointId(triangle.p2)
      val ptId3 = getOrCreatePointId(triangle.p3)
      TriangleCell(PointId(ptId1), PointId(ptId2), PointId(ptId3))
    }.toIndexedSeq
    TriangleMesh3D(pointsArray.take(pointCounter).toIndexedSeq, TriangleList(triangles))
  }
}
