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
package scalismo.io

import java.io.{ ByteArrayOutputStream, File, InputStream }

import org.scalatest.{ Matchers, FunSpec }
import org.scalatest.matchers.ShouldMatchers
import scalismo.geometry._
import scalismo.statisticalmodel.NDimensionalNormalDistribution

import scala.io.Source
import scala.language.implicitConversions

class LandmarkIOTests extends FunSpec with Matchers {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  implicit def inputStreamToSource(s: InputStream): Source = Source.fromInputStream(s)

  describe("Spray LandmarkIO") {

    val csvName = "/landmarks.csv"
    val csvUrl = getClass.getResource(csvName)
    def csvStream() = getClass.getResourceAsStream(csvName)

    val jsonName = "/landmarks.json"
    def jsonStream() = getClass.getResourceAsStream(jsonName)

    val jsonComplexName = "/landmarks2.json"
    def jsonComplexStream() = getClass.getResourceAsStream(jsonComplexName)
    /*
     * LEGACY LANDMARKS (csv format)
     */

    it("can read 3D landmarks in CSV format from a file") {
      val landmarksTry = LandmarkIO.readLandmarksCsv[_3D](new File(csvUrl.getPath))
      landmarksTry should be a 'Success

      val landmarks = landmarksTry.get
      landmarks.size should be(4)
      landmarks(3).point should be(Point(3.0, 1.0, 4.0))
    }

    it("can read 3D landmarks in CSV format from a stream") {
      val landmarksTry = LandmarkIO.readLandmarksCsvFromSource[_3D](csvStream())
      landmarksTry should be a 'Success

      val landmarks = landmarksTry.get
      landmarks.size should be(4)
      landmarks(3).point should be(Point(3.0, 1.0, 4.0))
    }

    it("can read 2D landmarks in CSV format from a file") {
      val landmarksTry = LandmarkIO.readLandmarksCsv[_2D](new File(csvUrl.getPath))
      landmarksTry should be a 'Success

      val landmarks = landmarksTry.get
      landmarks.size should be(4)
      landmarks(3).point should be(Point(3.0, 1.0))
    }

    it("can write a CSV file to disk and restore the landmarks") {
      val tmpFile = File.createTempFile("landmark", "txt")
      tmpFile.deleteOnExit()

      val landmarks = IndexedSeq(("first", Point(1.0, 2.0, 3.0)), ("second", Point(2.0, 1.0, 3.0))).map(t => Landmark(t._1, t._2))
      LandmarkIO.writeLandmarksCsv(tmpFile, landmarks) should be a 'Success

      val restoredLandmarksTry = LandmarkIO.readLandmarksCsv[_3D](tmpFile)
      restoredLandmarksTry should be a 'Success
      val restoredLandmarks = restoredLandmarksTry.get
      restoredLandmarks.size should be(landmarks.size)

      for ((landmark, restoredLandmark) <- landmarks.zip(restoredLandmarks)) {
        landmark should equal(restoredLandmark)
      }
    }

    /*
     * SIMPLE JSON LANDMARKS (no extensions)
     */

    def distWithDefaultVectors(d1: Float, d2: Float, d3: Float): NDimensionalNormalDistribution[_3D] = {
      val axes = List(Vector(1, 0, 0), Vector(0, 1, 0), Vector(0, 0, 1))
      val devs = List(d1, d2, d3)
      val data = axes zip devs
      NDimensionalNormalDistribution(Vector(0, 0, 0), data)
    }

    val jsonLm1 = Landmark("one", Point(1, 2, 3))
    val jsonLm2 = Landmark("two", Point(2, 3, 4), Some("Landmark two"), Some(distWithDefaultVectors(1, 4, 9)))
    val jsonLms = List(jsonLm1, jsonLm2)

    it("can serialize and deserialize simple landmarks using JSON") {
      val out = new ByteArrayOutputStream()
      LandmarkIO.writeLandmarksJsonToStream(out, jsonLms)
      val written = new String(out.toByteArray)
      val read = LandmarkIO.readLandmarksJsonFromSource[_3D](Source.fromString(written)).get
      read should equal(jsonLms)
    }

    it("can read simple landmarks from a JSON Stream") {
      val read = LandmarkIO.readLandmarksJsonFromSource[_3D](jsonStream()).get
      read should equal(jsonLms)
    }

    /*
     * COMPLEX JSON LANDMARKS (with extensions)
     * This example uses two additional (bogus) attributes: color, and visibility.
     * For simplicity, it uses an internal case class.
     */

    import spray.json.DefaultJsonProtocol._
    import spray.json._

    case class LMData(color: Option[String], visible: Boolean)
    implicit val LMDataProtocol = jsonFormat2(LMData)

    case class TestLandmark(lm: Landmark[_3D], data: LMData)

    val extLm1 = TestLandmark(jsonLm1, LMData(None, visible = false))
    val extLm2 = TestLandmark(jsonLm2, LMData(Some("red"), visible = true))
    val extLm2E = TestLandmark(jsonLm2, LMData(None, visible = false))

    val extLms = List(extLm1, extLm2)
    val extLmsE = List(extLm1, extLm2E)

    implicit val extEncode: LandmarkIO.ExtensionEncodeFunction[_3D, TestLandmark] = { tlm => (tlm.lm, Some(Map("test.ext" -> tlm.data.toJson))) }
    implicit val extDecode: LandmarkIO.ExtensionDecodeFunction[_3D, TestLandmark] = { (lm, json) =>
      val data = json.map(_.get("test.ext")).flatMap(_.map(_.convertTo[LMData]))
      TestLandmark(lm, data.getOrElse(LMData(None, visible = false)))
    }

    it("can serialize and deserialize complex landmarks using JSON") {
      val out = new ByteArrayOutputStream()
      LandmarkIO.writeLandmarksJsonToStream(out, extLms)
      val read = LandmarkIO.readLandmarksJsonFromSource[_3D, TestLandmark](Source.fromBytes(out.toByteArray)).get
      read should equal(extLms)
    }

    it("can read complex landmarks from a JSON Stream") {
      val read = LandmarkIO.readLandmarksJsonFromSource[_3D, TestLandmark](jsonComplexStream()).get
      read should equal(extLms)
    }

    it("can handle unexpected and missing extensions in JSON landmarks") {
      val cs = LandmarkIO.readLandmarksJsonFromSource[_3D, TestLandmark](jsonStream()).get
      cs should equal(extLmsE)
      val sc = LandmarkIO.readLandmarksJsonFromSource[_3D](jsonComplexStream()).get
      sc should equal(jsonLms)
    }
  }
}