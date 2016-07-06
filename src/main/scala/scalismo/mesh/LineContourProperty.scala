package scalismo.mesh

import scalismo.common.PointId

/**
 * Created by marcel on 15.09.15.
 */
trait LineContourProperty[A] {

  def topology : LineList

  /** access via line coordinates */
  def onContour(lineId: LineId, bcc: LineCoordinates): A

  /** access via line coordinates */
  def apply(lineId: LineId, bcc: LineCoordinates): A = onContour(lineId, bcc)

  /** map a surface property through a function */
  def map[B](f: A => B): LineContourProperty[B] = MappedContourProperty(this, f)
}

/** function indirection for contour access */
case class MappedContourProperty[A, B](values: LineContourProperty[A], f: A => B)
    extends LineContourProperty[B] {


  override def topology = values.topology

  /// access via line coordinates
  override def onContour(lineId: LineId, bcc: LineCoordinates): B = f(values.onContour(lineId, bcc))
}

case class ContourPointProperty[A](topology: LineList, pointData: IndexedSeq[A])(implicit val interpolator: Interpolator[A])
    extends LineContourProperty[A] {

  require(topology.pointIds.forall(id => pointData.isDefinedAt(id.id)), "Line topology is not compatible with data")

  def atPoint(pointId: PointId): A = pointData(pointId.id)

  def apply(pointId: PointId): A = pointData(pointId.id)

  override def onContour(lineId: LineId, bcc: LineCoordinates): A = {
    val t = topology.lines(lineId.id)
    val v1 = pointData(t.ptId1.id)
    val v2 = pointData(t.ptId2.id)
    bcc.interpolateProperty(v1, v2)
  }

  def map[B](f: A => B)(implicit interpolator: Interpolator[B]): ContourPointProperty[B] = {
    val newData = pointData map f
    ContourPointProperty(topology, newData)
  }
}

object ContourPointProperty {

  def averagedPointProperty[A](linetopology: LineList, property: LineContourProperty[A])(implicit ops: Interpolator[A]): ContourPointProperty[A] = {
    def averager(data: IndexedSeq[A]): A = {
      data.size match {
        case 0 => throw new Exception("averaging over empty set")
        case 1 => data.head
        case _ => ops.average(data.head, data.tail: _*)
      }
    }
    sampleContourProperty(linetopology, property, averager)
  }

  def sampleContourProperty[A](lineTopology: LineList, property: LineContourProperty[A], reducer: IndexedSeq[A] => A)(implicit ops: Interpolator[A]): ContourPointProperty[A] = {

    // get all data for a single vertex:
    def getVertex(pointId: PointId): A = {
      val lines = lineTopology.adjacentLinesForPoint(pointId)
      val vdata =
        for (t <- lines) yield {
          val localLineIndex = lineTopology.lines(t.id).pointIds.indexOf(pointId)
          property.onContour(t, LineCoordinates.canonical(localLineIndex))
        }
      reducer(vdata) // reduce to a single value
    }
    // do for each vertex
    val data = for (v <- lineTopology.pointIds) yield getVertex(v)
    ContourPointProperty(lineTopology, data)
  }
}
