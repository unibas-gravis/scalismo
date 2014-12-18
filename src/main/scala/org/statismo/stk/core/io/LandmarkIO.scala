package org.statismo.stk.core.io

import java.io._

import argonaut.Argonaut._
import argonaut.{CodecJson, Json, Parse}
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.statisticalmodel.NDimensionalNormalDistribution

import scala.language.implicitConversions
import scala.collection.immutable
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scalaz.{Failure => ZFailure, Success => ZSuccess}

object LandmarkIO {

  type ExtensionEncodeFunction[D <: Dim, A] = A => (Landmark[D], Option[Map[String, Json]])
  type ExtensionDecodeFunction[D <: Dim, A] = (Landmark[D], Option[Map[String, Json]]) => A

  private case class ExtLandmark[D <: Dim](lm: Landmark[D], exts: Option[Map[String, Json]])

  private case class Uncertainty(stdDevs: List[Float], pcs: List[List[Float]])

  private implicit val uncertaintyCodec: CodecJson[Uncertainty] = casecodec2(Uncertainty.apply _, Uncertainty.unapply _)("stddevs", "pcvectors")


  private object Uncertainty {
    def u2m[D <: Dim : NDSpace](u: Uncertainty): NDimensionalNormalDistribution[D] = {
      val dim = implicitly[NDSpace[D]].dimensionality
      val pcs: Seq[Vector[D]] = u.pcs.take(dim).map{l => Vector(l.take(dim).toArray)}
      val variances: Seq[Float] = u.stdDevs.take(dim).map(f => f * f)

      val mean: Vector[D] = Vector(Array.fill(dim)(0.0f))
      NDimensionalNormalDistribution(mean, pcs.zip(variances))
    }

    def m2u[D <: Dim : NDSpace](m: NDimensionalNormalDistribution[D]): Uncertainty = {
      val (pcs, variances) = m.principalComponents.unzip
      val stdDevs: List[Float] = variances.map(Math.sqrt(_).toFloat).toList
      val pcList: List[List[Float]] = pcs.map(v => v.data.toList).toList
      Uncertainty(stdDevs, pcList)
    }
  }


  private def landmarkCodec[D <: Dim : NDSpace]: CodecJson[ExtLandmark[D]] = {
    CodecJson(
      e => ("extensions" :=? e.exts) ->?:
        ("uncertainty" :=? e.lm.uncertainty.map(Uncertainty.m2u(_))) ->?:
        ("coordinates" := e.lm.point.data.toList) ->:
        ("description" :=? e.lm.description) ->?:
        ("id" := e.lm.name) ->: jEmptyObject,
      c => for {
        data <- (c --\ "coordinates").as[List[Float]]
        id <- (c --\ "id").as[String]
        desc <- (c --\ "description").as[Option[String]]
        unc <- (c --\ "uncertainty").as[Option[Uncertainty]]
        ext <- (c --\ "extensions").as[Option[Map[String, Json]]]
      } yield {
        ExtLandmark(Landmark(Point(data.toArray), id, desc, unc.map(u => Uncertainty.u2m(u))), ext)
      }
    )
  }

  object Sourceable {
    def create(asSourceFunction: () => Source): Sourceable = new Sourceable {
      override def asSource(): Source = asSourceFunction()
    }
    implicit def sourceAsSource(s: Source): Sourceable = create(() => s)
    implicit def fileAsSource(s: File): Sourceable = create(() => Source.fromFile(s))
    implicit def stringAsSource(s: String): Sourceable = create(() => Source.fromString(s))
    implicit def byteArrayAsSource(s: Array[Byte]): Sourceable = create(() => Source.fromBytes(s))
    implicit def inputStreamAsSource(s: InputStream): Sourceable = create(() => Source.fromInputStream(s))
  }

  trait Sourceable {
    def asSource(): Source
  }

  object Sinkable {
    def create(asSinkFunction: () => OutputStream): Sinkable = new Sinkable {
      override def asOutputStream(): OutputStream = asSinkFunction()
    }

    implicit def StreamAsSink(stream: OutputStream): Sinkable = create(() => stream)
    implicit def fileAsSink(file: File): Sinkable = create(() => new FileOutputStream(file))
  }

  trait Sinkable {
    def asOutputStream(): OutputStream
  }

  /* Convenience method if the "standard" landmarks are used.
   * This simply avoids having to specify the Landmark[D] type all the time.
   */
  def readLandmarksJson[D <: Dim : NDSpace](source: Sourceable) = readLandmarksJson[D, Landmark[D]](source)

  def readLandmarksJson[D <: Dim, A](sourceable: Sourceable)(implicit extDecode: ExtensionDecodeFunction[D, A], dimOps: NDSpace[D]): Try[List[A]] = {
    val source = sourceable.asSource()

    implicit val lmCodec = landmarkCodec[D]

    val result = Try {
      val stringData = source.getLines().mkString("\n")
      Parse.decodeValidation[List[ExtLandmark[D]]](stringData) match {
        case ZSuccess(extLms) => Success(extLms.map(e => extDecode(e.lm, e.exts)))
        case ZFailure(v) => Failure(new IllegalArgumentException(v))
      }
    }.flatten
    try {
      source.close()
    }
    result
  }

  def writeLandmarksJson[D <: Dim, A](output: Sinkable, landmarks: List[A])(implicit extEncode: ExtensionEncodeFunction[D, A], dimOps: NDSpace[D]): Try[Unit] = Try {
    val lms = landmarks.map(extEncode).map{case (lm, ext) => ExtLandmark(lm, ext)}
    writeLandmarksJsonRaw(output, lms)
  }.flatten

  private def writeLandmarksJsonRaw[D <: Dim : NDSpace](output: Sinkable, landmarks: List[ExtLandmark[D]]): Try[Unit] = {
    val writer = new PrintWriter(output.asOutputStream(), true)
    val result = Try {
      implicit val lmCodec = landmarkCodec[D]
      writer.println(landmarks.asJson.toString())
    }
    try {writer.close()}
    result
  }

  /** ******************************************************************************************************************
    * Legacy file format (.csv) support:
    *
    * label, x[, y[, z] ]
    * *****************************************************************************************************************/

  private def readLandmarksCsvRaw(source: Sourceable): Try[immutable.IndexedSeq[(String, Array[Float])]] = {
    val src = source.asSource()
    val result = Try {
      val landmarks = for (line <- src.getLines() if line.nonEmpty && line(0) != '#') yield {
        val elements = line.split(',')
        (elements(0).trim, elements.drop(1).take(3).map(_.toFloat))
      }
      landmarks.toIndexedSeq
    }
    try {
      src.close()
    }
    result
  }

  def readLandmarksCsv[D <: Dim : NDSpace](source: Sourceable): Try[immutable.IndexedSeq[Landmark[D]]] = {
    val items = implicitly[NDSpace[D]].dimensionality
    for (landmarks <- readLandmarksCsvRaw(source)) yield {
      for (landmark <- landmarks) yield Landmark(Point(landmark._2.take(items)), landmark._1)
    }
  }

  def writeLandmarksCsv[D <: Dim](sink: Sinkable, landmarks: IndexedSeq[Landmark[D]]): Try[Unit] = {
    Try {
      val out = new PrintWriter(sink.asOutputStream(), true)
      for (landmark <- landmarks) {
        val line = landmark.point.dimensionality match {
          case 1 => landmark.name.trim + "," + landmark.point(0) + ",0,0"
          case 2 => landmark.name.trim + "," + landmark.point(0) + "," + landmark.point(1) + ",0"
          case 3 => landmark.name.trim + "," + landmark.point(0) + "," + landmark.point(1) + "," + landmark.point(2)
          case _ => Failure(new Exception("Landmarks with dimensionality " + landmark.point.dimensionality + "not supported"))
        }
        out.println(line)
      }
      out.close()
    }
  }

}