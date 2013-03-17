package smptk.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.scalatest.FailureMessages
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException
import smptk.image.Geometry.{ CoordVector2D, CoordVector3D }

class LandmarkIOTest extends FunSpec with ShouldMatchers {
  describe("LandmarkIO") {

    val testUrl = getClass().getResource("/landmarks.txt").getPath()

    it("can read 3d landmarks from a testfile") {
      val landmarksOrError = LandmarkIO.readLandmarks3D(new File(testUrl))
      landmarksOrError.isSuccess should be(true)
      val landmarks = landmarksOrError.get
      landmarks.size should be(4)
      landmarks(3) should be(CoordVector3D(3., 1., 4.))
    }
    it("can read 2d landmarks from a testfile") {
      val landmarksOrError = LandmarkIO.readLandmarks2D(new File(testUrl))
      landmarksOrError.isSuccess should be(true)
      val landmarks = landmarksOrError.get
      landmarks.size should be(4)
      landmarks(3) should be(CoordVector2D(3., 1.))

    }

    it("can write a file to disk and restore the landmarks") {
      val tmpFile = File.createTempFile("landmark", "txt")
      val landmarks = IndexedSeq(CoordVector3D(1., 2., 3.), CoordVector3D(2., 1., 3.))
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