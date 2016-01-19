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

import java.io._

import scalismo.geometry._
import scalismo.statisticalmodel.NDimensionalNormalDistribution
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.immutable
import scala.io.Source
import scala.language.{ implicitConversions, reflectiveCalls }
import scala.util.{ Failure, Try }

object LandmarkIO {

  private case class ExtLandmark[D <: Dim](lm: Landmark[D], exts: Option[Map[String, JsValue]])

  private case class Uncertainty(stddevs: List[Float], pcvectors: List[List[Float]])

  private implicit val uncertaintyProtocol = jsonFormat2(Uncertainty.apply)

  type ExtensionEncodeFunction[D <: Dim, A] = A => (Landmark[D], Option[Map[String, JsValue]])
  type ExtensionDecodeFunction[D <: Dim, A] = (Landmark[D], Option[Map[String, JsValue]]) => A

  private implicit def u2m[D <: Dim: NDSpace](u: Uncertainty): NDimensionalNormalDistribution[D] = {
    val dim = implicitly[NDSpace[D]].dimensionality
    val pcs: Seq[Vector[D]] = u.pcvectors.take(dim).map { l => Vector(l.take(dim).toArray) }
    val variances: Seq[Float] = u.stddevs.take(dim).map(f => f * f)
    val mean: Vector[D] = Vector(Array.fill(dim)(0.0f))
    NDimensionalNormalDistribution(mean, pcs.zip(variances))
  }

  private implicit def m2u[D <: Dim: NDSpace](m: NDimensionalNormalDistribution[D]): Uncertainty = {
    val (pcs, variances) = m.principalComponents.unzip
    val stdDevs: List[Float] = variances.map(Math.sqrt(_).toFloat).toList
    val pcList: List[List[Float]] = pcs.map(v => v.toArray.toList).toList
    Uncertainty(stdDevs, pcList)
  }

  private case class LandmarkJsonFormat[D <: Dim: NDSpace]() extends JsonFormat[ExtLandmark[D]] {
    def write(l: ExtLandmark[D]) = {
      val fixedMap = Map("id" -> JsString(l.lm.id),
        "coordinates" -> arrayFormat[Float].write(l.lm.point.toArray))
      val descriptionMap = l.lm.description.map { d => Map("description" -> JsString(d)) }.getOrElse(Map())
      val uncertaintyMap = l.lm.uncertainty.map { u => Map("uncertainty" -> uncertaintyProtocol.write(u)) }.getOrElse(Map())
      val extensionsMap = l.exts.map(e => Map("extensions" -> JsObject(e))).getOrElse(Map())
      JsObject(fixedMap ++ descriptionMap ++ uncertaintyMap ++ extensionsMap)
    }

    def read(value: JsValue) = {
      val (id, coordinates) = value.asJsObject.getFields("id", "coordinates") match {
        case Seq(i, c) => (i.convertTo[String], c.convertTo[Array[Float]])
        case _ => throw new DeserializationException("No coordinates or landmark id Found")
      }
      val description = value.asJsObject.getFields("description").headOption.map(_.convertTo[String])
      val extensions = value.asJsObject.getFields("extensions").headOption.map(_.convertTo[Map[String, JsValue]])
      val uncertainty = value.asJsObject.getFields("uncertainty").headOption.map(_.convertTo[Uncertainty])
      ExtLandmark(Landmark[D](id, Point[D](coordinates), description, uncertainty.map(u => u2m(u))), extensions)
    }
  }

  /* Convenience methods if the "standard" landmarks are used.
   * This simply avoids having to specify the Landmark[D] type all the time.
   */
  def readLandmarksJson[D <: Dim: NDSpace](file: File): Try[immutable.Seq[Landmark[D]]] = {
    readLandmarksJson[D, Landmark[D]](file)
  }

  def readLandmarksJsonFromSource[D <: Dim: NDSpace](source: Source): Try[immutable.Seq[Landmark[D]]] = {
    readLandmarksJsonFromSource[D, Landmark[D]](source)
  }

  def readLandmarksJson[D <: Dim: NDSpace, A](file: File)(implicit extDecode: ExtensionDecodeFunction[D, A]): Try[immutable.Seq[A]] = {
    readLandmarksJsonFromSource(Source.fromFile(file))
  }

  def readLandmarksJsonFromSource[D <: Dim: NDSpace, A](source: Source)(implicit extDecode: ExtensionDecodeFunction[D, A]): Try[immutable.Seq[A]] = {
    implicit val e = LandmarkJsonFormat[D]()
    for {
      result <- Try {
        val stringData = source.getLines().mkString("\n")
        val extLms = JsonParser(stringData).convertTo[List[ExtLandmark[D]]]
        extLms.map(e => extDecode(e.lm, e.exts))
      }
      d <- Try {
        source.close()
      }
    } yield result
  }

  @deprecated("Use method with the same name and inverted order of parameters. This method wil be removed in future versions.", "0.10.0")
  def writeLandmarksJson[D <: Dim: NDSpace](file: File, landmarks: immutable.Seq[Landmark[D]]): Try[Unit] = writeLandmarksJson(landmarks, file)

  @deprecated("Use method with the same name and inverted order of parameters. This method wil be removed in future versions.", "0.10.0")
  def writeLandmarksJsonToStream[D <: Dim, A](stream: OutputStream, landmarks: immutable.Seq[A])(implicit extEncode: ExtensionEncodeFunction[D, A], ndSpace: NDSpace[D]): Try[Unit] = writeLandmarksJsonToStream(landmarks, stream)

  private def writeLandmarksJsonToStreamP[D <: Dim: NDSpace](stream: OutputStream, landmarks: immutable.Seq[ExtLandmark[D]])(implicit e: JsonFormat[ExtLandmark[D]]): Try[Unit] = {
    val writer = new PrintWriter(stream, true)
    val result = Try {
      writer.println(landmarks.toJson.toString())
    }
    Try {
      writer.close()
    }
    result
  }

  def writeLandmarksJson[D <: Dim: NDSpace](landmarks: immutable.Seq[Landmark[D]], file: File): Try[Unit] = {
    writeLandmarksJsonToStream[D, Landmark[D]](landmarks, new FileOutputStream(file))
  }

  def writeLandmarksJsonToStream[D <: Dim, A](landmarks: immutable.Seq[A], stream: OutputStream)(implicit extEncode: ExtensionEncodeFunction[D, A], ndSpace: NDSpace[D]): Try[Unit] = Try {
    val lms = landmarks.map(extEncode).map { case (lm, ext) => ExtLandmark(lm, ext) }
    implicit val e = LandmarkJsonFormat[D]()
    writeLandmarksJsonToStreamP(stream, lms)
  }.flatten

  /**
   * ******************************************************************************************************************
   * Legacy file format (.csv) support:
   *
   * label, x[, y[, z] ]
   * ****************************************************************************************************************
   */

  def readLandmarksCsv[D <: Dim: NDSpace](file: File): Try[immutable.Seq[Landmark[D]]] = {
    readLandmarksCsvFromSource(Source.fromFile(file))
  }

  def readLandmarksCsvFromSource[D <: Dim: NDSpace](source: Source): Try[immutable.Seq[Landmark[D]]] = {
    val items = implicitly[NDSpace[D]].dimensionality
    for (landmarks <- readLandmarksCsvRaw(source)) yield {
      for (landmark <- landmarks) yield Landmark(landmark._1, Point(landmark._2.take(items)))
    }
  }

  private def readLandmarksCsvRaw(source: Source): Try[immutable.Seq[(String, Array[Float])]] = {
    val result = Try {
      val landmarks = for (line <- source.getLines() if line.nonEmpty && line(0) != '#') yield {
        val elements = line.split(',')
        (elements(0).trim, elements.slice(1, 4).map(_.toFloat))
      }
      landmarks.toIndexedSeq
    }
    Try {
      source.close()
    }
    result
  }

  @deprecated("Use method with the same name and inverted order of parameters. This method wil be removed in future versions.", "0.10.0")
  def writeLandmarksCsv[D <: Dim](file: File, landmarks: immutable.Seq[Landmark[D]]): Try[Unit] = writeLandmarksCsv(landmarks, file)

  @deprecated("Use method with the same name and inverted order of parameters. This method wil be removed in future versions.", "0.10.0")
  def writeLandmarksCsvToStream[D <: Dim](stream: OutputStream, landmarks: immutable.Seq[Landmark[D]]): Try[Unit] = writeLandmarksCsvToStream(landmarks, stream)

  def writeLandmarksCsv[D <: Dim](landmarks: immutable.Seq[Landmark[D]], file: File): Try[Unit] = {
    writeLandmarksCsvToStream(landmarks, new FileOutputStream(file))
  }

  def writeLandmarksCsvToStream[D <: Dim](landmarks: immutable.Seq[Landmark[D]], stream: OutputStream): Try[Unit] = {
    Try {
      val out = new PrintWriter(stream, true)
      for (landmark <- landmarks) {
        val line = landmark.point.dimensionality match {
          case 1 => landmark.id.trim + "," + landmark.point(0) + ",0,0"
          case 2 => landmark.id.trim + "," + landmark.point(0) + "," + landmark.point(1) + ",0"
          case 3 => landmark.id.trim + "," + landmark.point(0) + "," + landmark.point(1) + "," + landmark.point(2)
          case _ => Failure(new Exception("Landmarks with dimensionality " + landmark.point.dimensionality + "not supported"))
        }
        out.println(line)
      }
      out.close()
    }
  }
}
