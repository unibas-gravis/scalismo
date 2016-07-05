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

import scalismo.common.{ PointId, UnstructuredPointsDomain, Cell }
import scalismo.geometry._

import scala.util.Random

/**
 * Represents a line segment defined by two points.
 */
case class LineCell(ptId1: PointId, ptId2: PointId) extends Cell {

  /** Identifiers of the points belonging to the cell*/
  val pointIds = IndexedSeq(ptId1, ptId2)

  /** Returns true if the given point identifier is part of the triangle cell*/
  def containsPoint(ptId: PointId) = ptId1 == ptId || ptId2 == ptId

  def toIndex2D = IntVector(ptId1.id, ptId2.id)
}

abstract class PolyLine[D <: Dim: NDSpace](val pointSet: UnstructuredPointsDomain[D], val topology: LineList) {

  //val position = PointProperty(topology, points)
  val lines = topology.lines
  val cells = lines

  lazy val boundingBox = pointSet.boundingBox

  /**
   * Area of the mesh surface.
   *
   *  The computed area is the sum of all the triangle cell areas.
   */
  lazy val length = lines.map(line => computeLineLength(line)).sum

  /**
   *  Returns a triangle mesh that is the image of this mesh by the given transform.
   *
   *  This method maps all mesh points to their images by the given transform while maintaining the same triangle cell relations.
   *
   *  @param transform A function that maps a given point to a new position. All instances of [[scalismo.registration.Transformation]] being descendants of <code>Function1[Point[_3D], Point[_3D] ]</code> are valid arguments.
   */
  def transform(transform: Point[D] => Point[D])(implicit creator: PolyLine.Create[D]): PolyLine[D] = {
    creator.createPolyLine(pointSet.transform(transform), topology)
  }

  def computeLineLength(t: LineCell): Double = {
    // compute are of the triangle using heron's formula
    val A = pointSet.point(t.ptId1)
    val B = pointSet.point(t.ptId2)
    (B - A).norm
  }
}

object PolyLine {

  /** Typeclass for creating domains of arbitrary dimensionality */
  trait Create[D <: Dim] {
    def createPolyLine(pointSet: UnstructuredPointsDomain[D], topology: LineList): PolyLine[D]
  }

  implicit object Create2D extends Create[_2D] {
    override def createPolyLine(pointSet: UnstructuredPointsDomain[_2D], topology: LineList) = {
      PolyLine2D(pointSet, topology)
    }
  }

  implicit object Create3D extends Create[_3D] {
    override def createPolyLine(pointSet: UnstructuredPointsDomain[_3D], topology: LineList) = {
      PolyLine3D(pointSet, topology)
    }
  }

  implicit def parametricToConcreteType2D(polyLine: PolyLine[_2D]): PolyLine2D = {
    polyLine.asInstanceOf[PolyLine2D]
  }

  implicit def parametricToConcreteType3D(polyLine: PolyLine[_3D]): PolyLine3D = {
    polyLine.asInstanceOf[PolyLine3D]
  }

  def apply[D <: Dim](points: UnstructuredPointsDomain[D], topology: LineList)(implicit creator: Create[D]): PolyLine[D] = {
    creator.createPolyLine(points, topology)
  }

  def enforceConsistentCellDirections[D <: Dim](polyLine: PolyLine[D])(implicit creator: PolyLine.Create[D]): PolyLine[D] = {

    def reorientRecursive(curLine: LineCell, reorientedLines: IndexedSeq[LineCell]): IndexedSeq[LineCell] = {
      if (reorientedLines.contains(curLine)) {
        reorientedLines
      } else {
        val adjLines = polyLine.topology.adjacentLinesForPoint(curLine.ptId2).map(id => polyLine.topology.lines(id.id))

        require(adjLines.size <= 2)

        val adjLine = if (adjLines.head == curLine || adjLines.head == LineCell(curLine.ptId2, curLine.ptId1)) adjLines.last else adjLines.head

        val newAdjLine = if (adjLine.ptId1 == curLine.ptId2) {
          adjLine
        } else {
          LineCell(adjLine.ptId2, adjLine.ptId1)
        }

        reorientRecursive(newAdjLine, reorientedLines :+ curLine)
      }
    }
    val consistentLineList = LineList(reorientRecursive(polyLine.lines.head, IndexedSeq[LineCell]()))
    creator.createPolyLine(polyLine.pointSet, consistentLineList)
  }
}

case class PolyLine2D(override val pointSet: UnstructuredPointsDomain[_2D], override val topology: LineList) extends PolyLine[_2D](pointSet, topology) {

  /** Get all cell normals as a surface property */
  lazy val cellNormals: LineProperty[Vector2D] = {
    LineProperty(topology, lines.map(computeCellNormal))
  }

  /** Get all vertex normals as a surface property */

  lazy val vertexNormals: ContourPointProperty[Vector2D] = {

    val pd = pointSet.pointIds.map { id =>
      val cellIds = topology.adjacentLinesForPoint(id)
      val n = cellIds.foldLeft(Vector.zeros[_2D])((acc, cellId) => acc + computeCellNormal(topology.lines(cellId.id)))
      n / cellIds.size: Vector2D
    }

    ContourPointProperty(topology, pd.toIndexedSeq)
  }

  private def computeCellNormal(cell: LineCell): Vector2D = {

    val pt1 = pointSet.point(cell.ptId1)
    val pt2 = pointSet.point(cell.ptId2)

    val d = pt2 - pt1
    val n = Vector(-d.y, d.x) / Vector(-d.y, d.x).norm
    n / n.norm
  }

}

case class PolyLine3D(override val pointSet: UnstructuredPointsDomain[_3D], override val topology: LineList) extends PolyLine[_3D](pointSet, topology) {

}

/** property constant per triangle */
case class LineProperty[A](topology: LineList, lineData: IndexedSeq[A])
    extends LineContourProperty[A] {
  require(lineData.size == topology.lines.size)

  override def onContour(lineId: LineId, bcc: LineCoordinates): A = lineData(lineId.id)
}
