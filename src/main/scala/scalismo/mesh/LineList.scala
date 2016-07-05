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

final case class LineId(id: Int) extends AnyVal {
  def isValid: Boolean = this != LineId.invalid
}

object LineId {
  val invalid = TriangleId(-1)
}

object LineList {
  val empty = LineList(IndexedSeq.empty[LineCell])
}

/**
 * A list of lines that make up a poly line
 */
case class LineList(lines: IndexedSeq[LineCell]) {

  val pointIds = extractRange(lines)
  val lineIds = lines.indices.map(i => LineId(i))

  def line(id: LineId): LineCell = lines(id.id)

  /**
   * triangles adjacent to each point
   */
  lazy val adjacentLinesForPoint: PointId => IndexedSeq[LineId] = {
    // list structure
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[LineId]
    val lineMap = emptyMapData.toMap

    for (lineId <- lineIds) {
      val line = lines(lineId.id)
      lineMap(line.ptId1) += lineId
      lineMap(line.ptId2) += lineId
    }
    val data = lineMap.mapValues(s => s.toSet) // make immutable

    val dataSeq = IndexedSeq.tabulate(pointIds.size) { i => data(pointIds(i)).toIndexedSeq }
    id => dataSeq(id.id)
  }

  /**
   *
   * points adjacent to a point
   */
  lazy val adjacentPointsForPoint: PointId => IndexedSeq[PointId] = {
    // all co-occurrences in triangle list: all points reachable by a link
    val emptyMapData = for (p <- pointIds) yield p -> collection.mutable.Set.empty[PointId]
    val pointMap = emptyMapData.toMap

    for (lineId <- lineIds) {
      val line = lines(lineId.id)
      pointMap(line.ptId1) ++= line.pointIds
      pointMap(line.ptId2) ++= line.pointIds
    }
    for (p <- pointIds) {
      pointMap(p) -= p
    }
    val mapData = pointMap.mapValues(s => s.toSet) // make immutable
    val seqData = IndexedSeq.tabulate(pointIds.size) { i => mapData(pointIds(i)).toIndexedSeq }
    id => seqData(id.id)
  }

  /**
   * lines connected to line via common point
   *
   */
  lazy val adjacentTrianglesForTriangle: LineId => IndexedSeq[LineId] = {
    // for each triangle get the 3 defining vertices, for each of those get all surrounding triangles, remove self
    val emptyMapData = for (lineId <- lineIds) yield lineId -> collection.mutable.Set.empty[LineId]
    val lineMap = emptyMapData.toMap

    for (lineId <- lineIds) {
      lineMap(lineId) ++= lines(lineId.id).pointIds.flatMap(p => adjacentLinesForPoint(p))
      lineMap(lineId) -= lineId
    }
    val mapData = lineMap.mapValues(s => s.toSet)
    val seqData = IndexedSeq.tabulate(lineIds.size) { i => mapData(lineIds(i)).toIndexedSeq }
    id => seqData(id.id)
  }

  /** points connected to a line, this information is contained in lines */
  lazy val adjacentPointsForLine: LineId => IndexedSeq[PointId] = {
    id => line(id).pointIds
  }

  private[this] def extractRange(lines: IndexedSeq[LineCell]): IndexedSeq[PointId] = {
    val min = lines.flatMap(t => t.pointIds).minBy(_.id)
    val max = lines.flatMap(t => t.pointIds).maxBy(_.id)
    (min.id to max.id).map(id => PointId(id))
  }

}

