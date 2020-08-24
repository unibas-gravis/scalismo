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

import breeze.linalg.DenseVector
import scalismo.geometry._
import scalismo.statisticalmodel.MultivariateNormalDistribution
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.io.Source
import scala.language.implicitConversions
import scala.util.{Failure, Try}

object LandmarkIO {

  private case class Uncertainty(stddevs: List[Double], pcvectors: List[List[Double]])

  implicit private val uncertaintyProtocol = jsonFormat2(Uncertainty.apply)

  implicit private def u2m(u: Uncertainty): MultivariateNormalDistribution = {
    val dim = u.stddevs.size
    val pcs: Seq[DenseVector[Double]] = u.pcvectors.take(dim).map { l =>
      DenseVector(l.take(dim).toArray)
    }
    val variances: Seq[Double] = u.stddevs.take(dim).map(f => f * f)
    val mean: DenseVector[Double] = DenseVector.zeros(dim)
    MultivariateNormalDistribution(mean, pcs.zip(variances))

  }

  implicit private def m2u(m: MultivariateNormalDistribution): Uncertainty = {
    val (pcs, variances) = m.principalComponents.unzip
    val stdDevs: List[Double] = variances.map(Math.sqrt(_)).toList
    val pcList: List[List[Double]] = pcs.map(v => v.toArray.toList).toList
    Uncertainty(stdDevs, pcList)
  }

  private case class LandmarkJsonFormat[D: NDSpace]() extends JsonFormat[Landmark[D]] {
    def write(l: Landmark[D]) = {
      val fixedMap = Map("id" -> JsString(l.id), "coordinates" -> arrayFormat[Double].write(l.point.toArray))
      val descriptionMap = l.description
        .map { d =>
          Map("description" -> JsString(d))
        }
        .getOrElse(Map())
      val uncertaintyMap = l.uncertainty
        .map { u =>
          Map("uncertainty" -> uncertaintyProtocol.write(u))
        }
        .getOrElse(Map())
      JsObject(fixedMap ++ descriptionMap ++ uncertaintyMap)
    }

    def read(value: JsValue) = {
      val (id, coordinates) = value.asJsObject.getFields("id", "coordinates") match {
        case Seq(i, c) => (i.convertTo[String], c.convertTo[Array[Double]])
        case _         => throw new DeserializationException("No coordinates or landmark id Found")
      }
      val description = value.asJsObject.getFields("description").headOption.map(_.convertTo[String])
      val uncertainty = value.asJsObject.getFields("uncertainty").headOption.map(_.convertTo[Uncertainty])
      Landmark[D](id, Point[D](coordinates), description, uncertainty.map(u => u2m(u)))
    }
  }

  /* Convenience methods if the "standard" landmarks are used.
   * This simply avoids having to specify the Landmark[D] type all the time.
   */

  def readLandmarksJson[D: NDSpace](file: File): Try[Seq[Landmark[D]]] = {
    readLandmarksJsonFromSource(Source.fromFile(file))
  }

  def readLandmarksJson1D(file: File): Try[Seq[Landmark[_1D]]] = {
    readLandmarksJsonFromSource[_1D](Source.fromFile(file))
  }

  def readLandmarksJson2D(file: File): Try[Seq[Landmark[_2D]]] = {
    readLandmarksJsonFromSource[_2D](Source.fromFile(file))
  }

  def readLandmarksJson3D(file: File): Try[Seq[Landmark[_3D]]] = {
    readLandmarksJsonFromSource[_3D](Source.fromFile(file))
  }

  def readLandmarksJsonFromSource[D: NDSpace](source: Source): Try[Seq[Landmark[D]]] = {
    implicit val e = LandmarkJsonFormat[D]()
    for {
      result <- Try {
        val stringData = source.getLines().mkString("\n")
        JsonParser(stringData).convertTo[List[Landmark[D]]]
      }
      d <- Try {
        source.close()
      }
    } yield result
  }

  def writeLandmarksJson[D: NDSpace](landmarks: Seq[Landmark[D]], file: File): Try[Unit] =
    writeLandmarksJsonToStream[D](landmarks, new FileOutputStream(file))

  private def writeLandmarksJsonToStreamP[D: NDSpace](landmarks: Seq[Landmark[D]], stream: OutputStream)(
    implicit
    e: JsonFormat[Landmark[D]]
  ): Try[Unit] = {
    val writer = new PrintWriter(stream, true)
    val result = Try {
      writer.println(landmarks.toJson.toString())
    }
    Try {
      writer.close()
    }
    result
  }

  def writeLandmarksJsonToStream[D: NDSpace](landmarks: Seq[Landmark[D]], stream: OutputStream): Try[Unit] =
    Try {
      implicit val e = LandmarkJsonFormat[D]()
      writeLandmarksJsonToStreamP(landmarks, stream)
    }.flatten

  /**
   * ******************************************************************************************************************
   * Legacy file format (.csv) support:
   *
   * label, x[, y[, z] ]
   * ****************************************************************************************************************
   */
  def readLandmarksCsv[D: NDSpace](file: File): Try[Seq[Landmark[D]]] = {
    readLandmarksCsvFromSource(Source.fromFile(file))
  }

  def readLandmarksCsvFromSource[D: NDSpace](source: Source): Try[Seq[Landmark[D]]] = {
    val items = implicitly[NDSpace[D]].dimensionality
    for (landmarks <- readLandmarksCsvRaw(source)) yield {
      for (landmark <- landmarks) yield Landmark(landmark._1, Point(landmark._2.take(items)))
    }
  }

  private def readLandmarksCsvRaw(source: Source): Try[Seq[(String, Array[Double])]] = {
    val result = Try {
      val landmarks = for (line <- source.getLines() if line.nonEmpty && line(0) != '#') yield {
        val elements = line.split(',')
        (elements(0).trim, elements.slice(1, 4).map(_.toDouble))
      }
      landmarks.toIndexedSeq
    }
    Try {
      source.close()
    }
    result
  }

  def writeLandmarksCsv[D](landmarks: Seq[Landmark[D]], file: File): Try[Unit] = {
    writeLandmarksCsvToStream(landmarks, new FileOutputStream(file))
  }

  def writeLandmarksCsvToStream[D](landmarks: Seq[Landmark[D]], stream: OutputStream): Try[Unit] = {
    Try {
      val out = new PrintWriter(stream, true)
      for (landmark <- landmarks) {
        val line = landmark.point.dimensionality match {
          case 1 => landmark.id.trim + "," + landmark.point(0) + ",0,0"
          case 2 => landmark.id.trim + "," + landmark.point(0) + "," + landmark.point(1) + ",0"
          case 3 => landmark.id.trim + "," + landmark.point(0) + "," + landmark.point(1) + "," + landmark.point(2)
          case _ =>
            Failure(new Exception("Landmarks with dimensionality " + landmark.point.dimensionality + "not supported"))
        }
        out.println(line)
      }
      out.close()
    }
  }
}
