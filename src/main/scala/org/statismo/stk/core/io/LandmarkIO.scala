package org.statismo.stk.core.io

import java.io.File
import scala.util.Try
import scala.io.Source
import scala.util.Success
import scala.util.Failure
import org.statismo.stk.core.geometry._

object LandmarkIO {

  /**
   * read a file of the form
   * label, x, y, z
   */
  private def readLandmarks(f: File): Try[IndexedSeq[(String, Float, Float, Float)]] = {
    Try {
      val src = Source.fromFile(f)
      val iter = src.getLines()
      val landmarks = for (line <- src.getLines if line.isEmpty() == false && line(0) != '#') yield {
        val elements = line.split(',')
        val (xStr, yStr, zStr) = (elements(1), elements(2), elements(3))
        (elements(0).trim, xStr.toFloat, yStr.toFloat, zStr.toFloat)
      }
      landmarks.toIndexedSeq
    }
  }

  def readLandmarks1D(f: File): Try[IndexedSeq[(String, Point[_1D])]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield (landmark._1, Point(landmark._2))
    }
  }

  def readLandmarks2D(f: File): Try[IndexedSeq[(String, Point[_2D])]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield (landmark._1, Point(landmark._2, landmark._3))
    }
  }

  def readLandmarks3D(f: File): Try[IndexedSeq[(String, Point[_3D])]] = {
    for (landmarks <- readLandmarks(f)) yield {
      for (landmark <- landmarks) yield (landmark._1, Point(landmark._2, landmark._3, landmark._4))
    }
  }

  def writeLandmarks[D <: Dim](f: File, landmarks: IndexedSeq[(String, Point[D])]): Try[Unit] = {
    Try {
      val out = new java.io.FileWriter(f)
      for (landmark <- landmarks) {
        val line = landmark._2.dimensionality match {
          case 1 => landmark._1.trim + "," + landmark._2(0) + ",0,0"
          case 2 => landmark._1.trim + "," + landmark._2(0) + "," + landmark._2(1) + ",0"
          case 3 => landmark._1.trim + "," + landmark._2(0) + "," + landmark._2(1) + "," + landmark._2(2)
          case _ => Failure(new Exception("Landmarks with dimensionality " + landmark._2.dimensionality + "not supported"))
        }
        out.write(line + "\n")
      }
      out.close()
    }
  }

}