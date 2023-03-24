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

import scalismo.common.UnstructuredPoints.Create.{CreateUnstructuredPoints2D, CreateUnstructuredPoints3D}
import scalismo.common.{BoxDomain, Cell, DiscreteDomain, DiscreteField, DomainWarp, PointId, UnstructuredPoints}
import scalismo.geometry._
import scalismo.transformations.Transformation

import scala.language.implicitConversions

/**
 * Represents a line segment defined by two points.
 */
case class LineCell(ptId1: PointId, ptId2: PointId) extends Cell {

  /** Identifiers of the points belonging to the cell */
  val pointIds = IndexedSeq(ptId1, ptId2)

  /** Returns true if the given point identifier is part of the line cell */
  def containsPoint(ptId: PointId): Boolean = ptId1 == ptId || ptId2 == ptId
}

trait LineMesh[D] extends DiscreteDomain[D] { // (val pointSet: UnstructuredPoints[D], val topology: LineList) {
  val topology: LineList
  val pointSet: UnstructuredPoints[D]

  val position: ContourPointProperty[Point[D]] = ContourPointProperty(topology, pointSet.pointSequence)
  val lines: IndexedSeq[LineCell] = topology.lines
  val cells: IndexedSeq[LineCell] = lines

  lazy val boundingBox: BoxDomain[D] = pointSet.boundingBox

  /**
   * Length of the line contour.
   *
   * The computed area is the sum of all the line cell areas.
   */
  lazy val length: Double = lines.map(line => computeLineLength(line)).sum

  /**
   * Returns a line mesh that is the image of this mesh by the given transform.
   *
   * This method maps all mesh points to their images by the given transform while maintaining the same line cell
   * relations.
   *
   * @param transform
   *   A function that maps a given point to a new position. All instances of
   *   [[scalismo.registration.transformation.Transformation]] being descendants of <code>Function1[Point[_3D],
   *   Point[_3D] ]</code> are valid arguments.
   */
  def transform(transform: Point[D] => Point[D])(implicit creator: LineMesh.Create[D]): LineMesh[D] = {
    creator.createLineMesh(pointSet.transform(transform), topology)
  }

  def computeLineLength(t: LineCell): Double = {
    val A = pointSet.point(t.ptId1)
    val B = pointSet.point(t.ptId2)
    (B - A).norm
  }
}

object LineMesh {

  def apply[D](points: UnstructuredPoints[D], topology: LineList)(implicit creator: Create[D]): LineMesh[D] = {
    creator.createLineMesh(points, topology)
  }

  /** Typeclass for creating domains of arbitrary dimensionality */
  trait Create[D] {
    def createLineMesh(pointSet: UnstructuredPoints[D], topology: LineList): LineMesh[D]
  }

  implicit object Create2D extends Create[_2D] {
    override def createLineMesh(pointSet: UnstructuredPoints[_2D], topology: LineList): LineMesh[_2D] = {
      LineMesh2D(pointSet, topology)
    }
  }

  implicit object Create3D extends Create[_3D] {
    override def createLineMesh(pointSet: UnstructuredPoints[_3D], topology: LineList): LineMesh[_3D] = {
      LineMesh3D(pointSet, topology)
    }
  }

  implicit def parametricToConcreteType2D(polyLine: LineMesh[_2D]): LineMesh2D = {
    polyLine.asInstanceOf[LineMesh2D]
  }

  implicit def parametricToConcreteType3D(polyLine: LineMesh[_3D]): LineMesh3D = {
    polyLine.asInstanceOf[LineMesh3D]
  }

  def enforceConsistentCellDirections[D](lineMesh: LineMesh[D])(implicit creator: LineMesh.Create[D]): LineMesh[D] = {

    @scala.annotation.tailrec
    def reorientRecursive(curLine: LineCell, reorientedLines: IndexedSeq[LineCell]): IndexedSeq[LineCell] = {
      if (reorientedLines.contains(curLine)) {
        reorientedLines
      } else {
        val adjLines = lineMesh.topology.adjacentLinesForPoint(curLine.ptId2).map(id => lineMesh.topology.lines(id.id))

        require(adjLines.size <= 2)

        val adjLine =
          if (adjLines.head == curLine || adjLines.head == LineCell(curLine.ptId2, curLine.ptId1)) adjLines.last
          else adjLines.head

        val newAdjLine = if (adjLine.ptId1 == curLine.ptId2) {
          adjLine
        } else {
          LineCell(adjLine.ptId2, adjLine.ptId1)
        }

        reorientRecursive(newAdjLine, reorientedLines :+ curLine)
      }
    }
    val consistentLineList = LineList(reorientRecursive(lineMesh.lines.head, IndexedSeq[LineCell]()))
    creator.createLineMesh(lineMesh.pointSet, consistentLineList)
  }

  implicit object domainWarp2D extends DomainWarp[_2D, LineMesh] {

    /**
     * Warp the points of the domain of the discrete field and turn it into the warped domain
     */
    override def transformWithField(
      domain: LineMesh[_2D],
      warpField: DiscreteField[_2D, LineMesh, EuclideanVector[_2D]]
    ): LineMesh[_2D] = {

      require(domain.pointSet.numberOfPoints == warpField.domain.pointSet.numberOfPoints)

      val newPoints = for ((p, v) <- warpField.pointsWithValues) yield { p + v }
      LineMesh2D(CreateUnstructuredPoints2D.create(newPoints.toIndexedSeq), domain.topology)
    }

    override def transform(mesh: LineMesh[_2D], transformation: Transformation[_2D]): LineMesh[_2D] = {
      mesh.transform(transformation)
    }
  }

  implicit object domainWarp3D extends DomainWarp[_3D, LineMesh] {

    /**
     * Warp the points of the domain of the discrete field and turn it into the warped domain
     */
    override def transformWithField(
      domain: LineMesh[_3D],
      warpField: DiscreteField[_3D, LineMesh, EuclideanVector[_3D]]
    ): LineMesh[_3D] = {

      require(domain.pointSet.numberOfPoints == warpField.domain.pointSet.numberOfPoints)

      val newPoints = for ((p, v) <- warpField.pointsWithValues) yield { p + v }
      LineMesh3D(CreateUnstructuredPoints3D.create(newPoints.toIndexedSeq), domain.topology)
    }

    override def transform(mesh: LineMesh[_3D], transformation: Transformation[_3D]): LineMesh[_3D] = {
      mesh.transform(transformation)
    }
  }

}

case class LineMesh2D(override val pointSet: UnstructuredPoints[_2D], override val topology: LineList)
    extends LineMesh[_2D] {

  /** Get all cell normals as a surface property */
  lazy val cellNormals: LineProperty[EuclideanVector[_2D]] = {
    LineProperty(topology, lines.map(computeCellNormal))
  }

  /** Get all vertex normals as a surface property */
  lazy val vertexNormals: ContourPointProperty[EuclideanVector[_2D]] = {

    ContourPointProperty.averagedPointProperty(topology, cellNormals)
  }

  private def computeCellNormal(cell: LineCell): EuclideanVector[_2D] = {

    val pt1 = pointSet.point(cell.ptId1)
    val pt2 = pointSet.point(cell.ptId2)

    val d = pt2 - pt1
    val n = EuclideanVector(-d.y, d.x) / EuclideanVector(-d.y, d.x).norm
    n / n.norm
  }

}

case class LineMesh3D(override val pointSet: UnstructuredPoints[_3D], override val topology: LineList)
    extends LineMesh[_3D] {}

/** property constant per line */
case class LineProperty[A](topology: LineList, lineData: IndexedSeq[A]) extends LineContourProperty[A] {
  require(lineData.size == topology.lines.size)

  override def onContour(lineId: LineId, bcc: LineCoordinates): A = lineData(lineId.id)
}
