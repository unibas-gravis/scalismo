package smptk.io

import java.io.File
import scala.util.Try
import smptk.image.Geometry._
import scala.io.Source
import smptk.image.CoordVector
import scala.util.Success
import scala.util.Failure

object LandmarkIO {

  /**
   * read a file of the form
   * label, x, y, z
   */
  private def readLandmarks(f: File): Try[IndexedSeq[(Double, Double, Double)]] = {
    val src = Source.fromFile(f)
    val iter = src.getLines()
    Try {
      val landmarks = for (line <- src.getLines if line.isEmpty() == false && line(0) != '#') yield {
        val elements = line.split(',')
        val (xStr, yStr, zStr) = (elements(0), elements(1), elements(2))
        (xStr.toDouble, yStr.toDouble, zStr.toDouble)
      }
      landmarks.toIndexedSeq
    }
  }

  def readLandmarks1D(f: File): Try[IndexedSeq[Point1D]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield CoordVector1D(landmark._1)
    }
  }

  def readLandmarks2D(f: File): Try[IndexedSeq[Point2D]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield CoordVector2D(landmark._1, landmark._2)
    }
  }

  def readLandmarks3D(f: File): Try[IndexedSeq[Point3D]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield CoordVector3D(landmark._1, landmark._2, landmark._3)
    }
  }

  def writeLandmarks[CV[_] <: CoordVector[_]](f : File, landmarks : IndexedSeq[CV[Double]]) : Try[Unit] = { 
    Try { 
    	val out = new java.io.FileWriter(f)
    	for ((landmark, num) <- landmarks.zipWithIndex) {
    	  val line = landmark.dimensionality match { 
    	    case 1 => "landmark-"+num +"," + landmark(0) +",0,0"
    	    case 2 => "landmark-"+num +"," + landmark(0) +"," +landmark(1) + ",0"
    	    case 3 => "landmark-"+num +"," + landmark(0) +"," +landmark(1) + "," + landmark(2)
    	    case _ => Failure(new Exception("Landmarks with dimensionality "+landmark.dimensionality +"not supported"))
    	  }
    	  out.write(line +"\n")
    	}
    	out.close()
    }
  }
  
}