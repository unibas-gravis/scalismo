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
package scalismo.tetramesh

import scalismo.common.PointId

/** express a tetrahedriangulation, contains only tetrahedron information, no points */
case class TetrahedralList(tetrahedrons: IndexedSeq[TetrahedralCell]) {

  val pointIds = extractRange(tetrahedrons)
  val tetrahedronIds = tetrahedrons.indices.map(i => TetrahedronId(i))

  def tetrahedron(id: TetrahedronId): TetrahedralCell = tetrahedrons(id.id)

  /** tetrahedrons adjacent to each point */
  lazy val adjacentTetrahedronsForPoint: PointId => IndexedSeq[TetrahedronId] = {
    // list structure
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[TetrahedronId]
    val tetrahedronMap = emptyMapData.toMap

    for (t <- tetrahedronIds) {
      val tetrahedron = tetrahedrons(t.id)
      tetrahedronMap(tetrahedron.ptId1) += t
      tetrahedronMap(tetrahedron.ptId2) += t
      tetrahedronMap(tetrahedron.ptId3) += t
      tetrahedronMap(tetrahedron.ptId4) += t

    }
    val data = tetrahedronMap.mapValues(s => s.toSet) // make immutable

    val dataSeq = IndexedSeq.tabulate(pointIds.size) { i => data(pointIds(i)).toIndexedSeq }
    id => dataSeq(id.id)
  }

  /** points adjacent to a point */
  lazy val adjacentPointsForPoint: PointId => IndexedSeq[PointId] = {
    // all co-occurrences in tetrahedron list: all points reachable by a link
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[PointId]
    val pointMap = emptyMapData.toMap

    for (t <- tetrahedronIds) {
      val tetrahedron = tetrahedrons(t.id)
      pointMap(tetrahedron.ptId1) ++= tetrahedron.pointIds
      pointMap(tetrahedron.ptId2) ++= tetrahedron.pointIds
      pointMap(tetrahedron.ptId3) ++= tetrahedron.pointIds
      pointMap(tetrahedron.ptId4) ++= tetrahedron.pointIds
    }
    for (p <- pointIds) {
      pointMap(p) -= p
    }
    val mapData = pointMap.mapValues(s => s.toSet) // make immutable
    val seqData = IndexedSeq.tabulate(pointIds.size) { i => mapData(pointIds(i)).toIndexedSeq }
    id => seqData(id.id)
  }

  /** tetrahedrons connected to tetrahedron via common point */
  lazy val adjacentTetrahedronsForTetrahedron: TetrahedronId => IndexedSeq[TetrahedronId] = {
    // for each tetrahedron get the 4 defining vertices, for each of those get all surrounding tetrahedrons, remove self
    val emptyMapData = for (t <- tetrahedronIds) yield t -> collection.mutable.Set.empty[TetrahedronId]
    val tetrahedronmap = emptyMapData.toMap

    for (t <- tetrahedronIds) {
      tetrahedronmap(t) ++= tetrahedrons(t.id).pointIds.flatMap(p => adjacentTetrahedronsForPoint(p))
      tetrahedronmap(t) -= t
    }
    val mapData = tetrahedronmap.mapValues(s => s.toSet)
    val seqData = IndexedSeq.tabulate(tetrahedronIds.size) { i => mapData(tetrahedronIds(i)).toIndexedSeq }
    id => seqData(id.id)
  }

  /** points connected to a tetrahedron, this information is contained in tetrahedrons */
  lazy val adjacentPointsForTetrahedron: TetrahedronId => IndexedSeq[PointId] = {
    id => tetrahedron(id).pointIds
  }

  private[this] def extractRange(tetrahedrons: IndexedSeq[TetrahedralCell]): IndexedSeq[PointId] = {
    if (tetrahedrons.isEmpty) {
      IndexedSeq[PointId]()
    } else {
      val min = tetrahedrons.flatMap(t => t.pointIds).minBy(_.id)
      val max = tetrahedrons.flatMap(t => t.pointIds).maxBy(_.id)
      (min.id to max.id).map(id => PointId(id))
    }
  }
}

final case class TetrahedronId(id: Int) extends AnyVal {
  def isValid: Boolean = this != TetrahedronId.invalid
}

object TetrahedronId {
  val invalid = TetrahedronId(-1)
}

object TetrahedralList {
  val empty = TetrahedralList(IndexedSeq.empty[TetrahedralCell])
}
