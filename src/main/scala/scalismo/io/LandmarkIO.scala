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

import scala.io.Source
import scala.language.implicitConversions
import scala.util.{Failure, Try}

object LandmarkIO {

  private case class Uncertainty(stddevs: List[Double], pcvectors: List[List[Double]]) {
    def asJson(): ujson.Obj = {
      ujson.Obj(
        "stddevs" -> ujson.Arr.from(stddevs),
        "pcvectors" -> ujson.Arr.from(pcvectors)
      )
    }

  }
  private object Uncertainty {
    def fromJson(value: ujson.Value): Try[Uncertainty] = Try {
      val stddevs = value("stddevs").arr.toList.map(_.num)
      val pcvectors = value("pcvectors").arr.toList.map(v => v.arr.toList.map(_.num))
      Uncertainty(stddevs, pcvectors)
    }

  }

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

  private case class LandmarkJsonSerializer[D: NDSpace]() {

    def toJson(l: Landmark[D]): ujson.Obj = {

      val pointItems: List[(String, ujson.Value)] = List(
        "id" -> ujson.Str(l.id),
        "coordinates" -> ujson.Arr.from(l.point.toArray)
      )

      val descriptionItems: List[(String, ujson.Value)] = l.description match {
        case Some(d) => List("description" -> ujson.Str(d))
        case None    => Nil
      }
      val uncertaintyItems: List[(String, ujson.Value)] = l.uncertainty match {
        case Some(u) => List("uncertainty" -> m2u(u).asJson())
        case None    => Nil
      }

      ujson.Obj.from(pointItems ++ descriptionItems ++ uncertaintyItems)
    }

    def fromJson(jsonValue: ujson.Value): Try[Landmark[D]] = Try {
      val id = jsonValue("id").str
      val coordinates = {
        jsonValue("coordinates").arr.map(_.num).toArray
      }

      val description = jsonValue.obj.get("description").map(_.str)
      val uncertainty = jsonValue.obj.get("uncertainty") match {
        case Some(uncertainty) => Uncertainty.fromJson(uncertainty).toOption
        case None              => None
      }
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
    for {
      result <- Try {
        val stringData = source.getLines().mkString("\n")
        val jsonData = ujson.read(stringData)
        jsonData.arr.map(obj => LandmarkJsonSerializer[D]().fromJson(obj).get).toSeq
      }
      d <- Try {
        source.close()
      }
    } yield result
  }

  def writeLandmarksJson[D: NDSpace](landmarks: Seq[Landmark[D]], file: File): Try[Unit] =
    writeLandmarksJsonToStream[D](landmarks, new FileOutputStream(file))

  private def writeLandmarksJsonToStreamP[D: NDSpace](landmarks: Seq[Landmark[D]], stream: OutputStream): Try[Unit] = {
    val writer = new PrintWriter(stream, true)
    val result = Try {
      val jsonObjs = for (lm <- landmarks) yield LandmarkJsonSerializer[D]().toJson(lm)
      val jsonValue = ujson.Arr.from(jsonObjs)
      writer.println(jsonValue.toString())
    }
    Try {
      writer.close()
    }
    result
  }

  def writeLandmarksJsonToStream[D: NDSpace](landmarks: Seq[Landmark[D]], stream: OutputStream): Try[Unit] =
    for {
      _ <- writeLandmarksJsonToStreamP(landmarks, stream)
    } yield ()

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
