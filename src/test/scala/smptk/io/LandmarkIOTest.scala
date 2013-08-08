package smptk.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.scalatest.FailureMessages
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException
import smptk.geometry._
import smptk.geometry.implicits._

class LandmarkIOTest extends FunSpec with ShouldMatchers {
  describe("LandmarkIO") {

    val testUrl = getClass().getResource("/landmarks.txt").getPath()

    it("can read 3d landmarks from a testfile") {
      val landmarksOrError = LandmarkIO.readLandmarks3D(new File(testUrl))
      landmarksOrError.isSuccess should be(true)
      val landmarks = landmarksOrError.get
      landmarks.size should be(4)
      landmarks(3) should be(Point3D(3.0, 1.0, 4.0))
    }
    it("can read 2d landmarks from a testfile") {
      val landmarksOrError = LandmarkIO.readLandmarks2D(new File(testUrl))
      landmarksOrError.isSuccess should be(true)
      val landmarks = landmarksOrError.get
      landmarks.size should be(4)
      landmarks(3) should be(Point2D(3.0, 1.0))

    }

    it("can write a file to disk and restore the landmarks") {
      val tmpFile = File.createTempFile("landmark", "txt")
      val landmarks = IndexedSeq(Point3D(1.0, 2.0, 3.0), Point3D(2.0, 1.0, 3.0))
      val maybeFailure = LandmarkIO.writeLandmarks(tmpFile, landmarks)
      maybeFailure.isSuccess should be(true)

      val restoredLandmarksOrFailure = LandmarkIO.readLandmarks3D(tmpFile)
      println(restoredLandmarksOrFailure)
      restoredLandmarksOrFailure.isSuccess should be(true)
      val restoredLandmarks = restoredLandmarksOrFailure.get

      landmarks.size should be(restoredLandmarks.size)

      for ((landmark, restoredLandmark) <- landmarks.zip(restoredLandmarks)) {
        landmark should equal(restoredLandmark)
      }
      tmpFile.deleteOnExit()
    }

  }
}