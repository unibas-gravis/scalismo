package smptk.io

import java.io.File
import scala.util.Try
import scala.io.Source
import scala.util.Success
import scala.util.Failure
import smptk.geometry._

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
        val (xStr, yStr, zStr) = (elements(1), elements(2), elements(3))
        (xStr.toDouble, yStr.toDouble, zStr.toDouble)
      }
      landmarks.toIndexedSeq
    }
  }

  def readLandmarks1D(f: File): Try[IndexedSeq[Point1D]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield Point1D(landmark._1)
    }
  }

  def readLandmarks2D(f: File): Try[IndexedSeq[Point2D]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield Point2D(landmark._1, landmark._2)
    }
  }

  def readLandmarks3D(f: File): Try[IndexedSeq[Point3D]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield Point3D(landmark._1, landmark._2, landmark._3)
    }
  }

  def writeLandmarks[D <: Dim](f : File, landmarks : IndexedSeq[Point[D]]) : Try[Unit] = { 
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