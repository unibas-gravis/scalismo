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
package scalismo.mesh

import scalismo.common.PointId

/** List of tetrahedrons used to define the tetrahedralization in the [[TetrahedralMesh]]. */
case class TetrahedralList(tetrahedrons: IndexedSeq[TetrahedralCell]) {

  val pointIds = extractPointIds(tetrahedrons)
  val tetrahedronIds = tetrahedrons.indices.map(i => TetrahedronId(i))

  def tetrahedron(id: TetrahedronId): TetrahedralCell = tetrahedrons(id.id)

  /** Function returning a list of all ids of tetrahedrons containing the point id. */
  lazy val adjacentTetrahedronsForPoint: PointId => IndexedSeq[TetrahedronId] = {
    // list structure
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[TetrahedronId]
    val tetrahedronMap = emptyMapData.toMap

    for (t <- tetrahedronIds) {
      val tetrahedralCell = tetrahedron(t)
      tetrahedronMap(tetrahedralCell.ptId1) += t
      tetrahedronMap(tetrahedralCell.ptId2) += t
      tetrahedronMap(tetrahedralCell.ptId3) += t
      tetrahedronMap(tetrahedralCell.ptId4) += t
    }

    val data = tetrahedronMap.mapValues(s => s.toSet) // make immutable

    val dataSeq = IndexedSeq.tabulate(pointIds.size) { i =>
      data(pointIds(i)).toIndexedSeq
    }
    id => dataSeq(id.id)
  }

  /** List of neighboring points for a point id connected directly with an edge. */
  lazy val adjacentPointsForPoint: PointId => IndexedSeq[PointId] = {
    // all co-occurrences in tetrahedron list: all points reachable by a link
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[PointId]
    val pointMap = emptyMapData.toMap

    for (t <- tetrahedronIds) {
      val tetrahedralCell = tetrahedron(t)
      pointMap(tetrahedralCell.ptId1) ++= tetrahedralCell.pointIds
      pointMap(tetrahedralCell.ptId2) ++= tetrahedralCell.pointIds
      pointMap(tetrahedralCell.ptId3) ++= tetrahedralCell.pointIds
      pointMap(tetrahedralCell.ptId4) ++= tetrahedralCell.pointIds
    }
    for (p <- pointIds) {
      pointMap(p) -= p
    }
    val mapData = pointMap.mapValues(s => s.toSet) // make immutable
    val seqData = IndexedSeq.tabulate(pointIds.size) { i =>
      mapData(pointIds(i)).toIndexedSeq
    }
    id => seqData(id.id)
  }

  /** List of neighboring tetrahedrons of a tetrahedron, having at least one common point */
  lazy val adjacentTetrahedronsForTetrahedron: TetrahedronId => IndexedSeq[TetrahedronId] = {
    // for each tetrahedron get the 4 defining vertices, for each of those get all surrounding tetrahedrons, remove self
    val emptyMapData = for (t <- tetrahedronIds) yield t -> collection.mutable.Set.empty[TetrahedronId]
    val tetrahedronmap = emptyMapData.toMap

    for (t <- tetrahedronIds) {
      tetrahedronmap(t) ++= tetrahedrons(t.id).pointIds.flatMap(p => adjacentTetrahedronsForPoint(p))
      tetrahedronmap(t) -= t
    }
    val mapData = tetrahedronmap.mapValues(s => s.toSet)
    val seqData = IndexedSeq.tabulate(tetrahedronIds.size) { i =>
      mapData(tetrahedronIds(i)).toIndexedSeq
    }
    id => seqData(id.id)
  }

  /** Points contained in a tetrahedron, this information is contained in tetrahedron cells. */
  lazy val adjacentPointsForTetrahedron: TetrahedronId => IndexedSeq[PointId] = { id =>
    tetrahedron(id).pointIds
  }

  /** Create a list of all point ids contained in at least one tetrahedral cell. */
  private[this] def extractPointIds(tetrahedrons: IndexedSeq[TetrahedralCell]): IndexedSeq[PointId] = {
    tetrahedrons.flatMap(t => t.pointIds).map(_.id).distinct.sorted.map(id => PointId(id))
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
