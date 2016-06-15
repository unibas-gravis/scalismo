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

import scalismo.common.PointId

/** express a triangulation, contains only triangle information, no points */
case class TriangleList(triangles: IndexedSeq[TriangleCell]) {

  val pointIds = extractRange(triangles)
  val triangleIds = triangles.indices.map(i => TriangleId(i))

  def triangle(id: TriangleId): TriangleCell = triangles(id.id)

  /** triangles adjacent to each point */
  lazy val adjacentTrianglesForPoint: PointId => IndexedSeq[TriangleId] = {
    // list structure
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[TriangleId]
    val triangleMap = emptyMapData.toMap

    for (t <- triangleIds) {
      val triangle = triangles(t.id)
      triangleMap(triangle.ptId1) += t
      triangleMap(triangle.ptId2) += t
      triangleMap(triangle.ptId3) += t
    }
    val data = triangleMap.mapValues(s => s.toSet) // make immutable

    val dataSeq = IndexedSeq.tabulate(pointIds.size) { i => data(pointIds(i)).toIndexedSeq }
    id => dataSeq(id.id)
  }

  /** points adjacent to a point */
  lazy val adjacentPointsForPoint: PointId => IndexedSeq[PointId] = {
    // all co-occurrences in triangle list: all points reachable by a link
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[PointId]
    val pointMap = emptyMapData.toMap

    for (t <- triangleIds) {
      val triangle = triangles(t.id)
      pointMap(triangle.ptId1) ++= triangle.pointIds
      pointMap(triangle.ptId2) ++= triangle.pointIds
      pointMap(triangle.ptId3) ++= triangle.pointIds
    }
    for (p <- pointIds) {
      pointMap(p) -= p
    }
    val mapData = pointMap.mapValues(s => s.toSet) // make immutable
    val seqData = IndexedSeq.tabulate(pointIds.size) { i => mapData(pointIds(i)).toIndexedSeq }
    id => seqData(id.id)
  }

  /** triangles connected to triangle via common point */
  lazy val adjacentTrianglesForTriangle: TriangleId => IndexedSeq[TriangleId] = {
    // for each triangle get the 3 defining vertices, for each of those get all surrounding triangles, remove self
    val emptyMapData = for (t <- triangleIds) yield t -> collection.mutable.Set.empty[TriangleId]
    val triangleMap = emptyMapData.toMap

    for (t <- triangleIds) {
      triangleMap(t) ++= triangles(t.id).pointIds.flatMap(p => adjacentTrianglesForPoint(p))
      triangleMap(t) -= t
    }
    val mapData = triangleMap.mapValues(s => s.toSet)
    val seqData = IndexedSeq.tabulate(triangleIds.size) { i => mapData(triangleIds(i)).toIndexedSeq }
    id => seqData(id.id)
  }

  /** points connected to a triangle, this information is contained in triangles */
  lazy val adjacentPointsForTriangle: TriangleId => IndexedSeq[PointId] = {
    id => triangle(id).pointIds
  }

  private[this] def extractRange(triangles: IndexedSeq[TriangleCell]): IndexedSeq[PointId] = {
    if(triangles.isEmpty){
      IndexedSeq[PointId]()
    } else {
      val min = triangles.flatMap(t => t.pointIds).minBy(_.id)
      val max = triangles.flatMap(t => t.pointIds).maxBy(_.id)
      (min.id to max.id).map(id => PointId(id))
    }
  }
}

final case class TriangleId(id: Int) extends AnyVal {
  def isValid: Boolean = this != TriangleId.invalid
}

object TriangleId {
  val invalid = TriangleId(-1)
}

object TriangleList {
  val empty = TriangleList(IndexedSeq.empty[TriangleCell])
}
