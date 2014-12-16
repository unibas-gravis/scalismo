package org.statismo.stk.core.io

import java.io.File

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.geometry.Point

import scala.language.implicitConversions


class LandmarkIOTests extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  describe("LandmarkIO") {

    val testUrl = getClass.getResource("/landmarks.txt").getPath

    it("can read 3D landmarks from a file") {
      val landmarksTry = LandmarkIO.readLandmarks3D(new File(testUrl))
      landmarksTry should be a 'Success

      val landmarks = landmarksTry.get
      landmarks.size should be(4)
      landmarks(3)._2 should be(Point(3.0, 1.0, 4.0))
    }

    it("can read 2D landmarks from a file") {
      val landmarksTry = LandmarkIO.readLandmarks2D(new File(testUrl))
      landmarksTry should be a 'Success

      val landmarks = landmarksTry.get
      landmarks.size should be(4)
      landmarks(3)._2 should be(Point(3.0, 1.0))
    }

    it("can write a file to disk and restore the landmarks") {
      val tmpFile = File.createTempFile("landmark", "txt")
      tmpFile.deleteOnExit()

      val landmarks = IndexedSeq(("first", Point(1.0, 2.0, 3.0)), ("second", Point(2.0, 1.0, 3.0)))
      LandmarkIO.writeLandmarks(tmpFile, landmarks) should be a 'Success

      val restoredLandmarksTry = LandmarkIO.readLandmarks3D(tmpFile)
      restoredLandmarksTry should be a 'Success
      val restoredLandmarks = restoredLandmarksTry.get
      restoredLandmarks.size should be(landmarks.size)

      for ((landmark, restoredLandmark) <- landmarks.zip(restoredLandmarks)) {
        landmark should equal(restoredLandmark)
      }
    }
  }
}