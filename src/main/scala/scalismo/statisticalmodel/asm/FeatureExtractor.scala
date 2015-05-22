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
package scalismo.statisticalmodel.asm

import breeze.linalg.DenseVector
import ncsa.hdf.`object`.Group
import scalismo.geometry.{ Vector, Point, _3D }
import scalismo.io.HDF5File
import scalismo.mesh.TriangleMesh

import scala.collection.immutable
import scala.util.{ Failure, Try }

trait FeatureExtractor extends Function3[PreprocessedImage, TriangleMesh, Point[_3D], Option[DenseVector[Float]]] with HasIOMetadata

trait FeatureExtractorIOHandler extends IOHandler[FeatureExtractor]

object FeatureExtractorIOHandlers extends IOHandlers[FeatureExtractor, FeatureExtractorIOHandler] {
  register(NormalDirectionFeatureExtractorIOHandler)
}

object NormalDirectionFeatureExtractor {
  val IOIdentifier = "builtin::NormalDirection"
  val IOMetadata_1_0 = IOMetadata(IOIdentifier, 1, 0)
  val IOMetadata_Default = IOMetadata_1_0
}

case class NormalDirectionFeatureExtractor(numberOfPoints: Int, spacing: Float, override val ioMetadata: IOMetadata = NormalDirectionFeatureExtractor.IOMetadata_Default) extends FeatureExtractor {
  override def apply(image: PreprocessedImage, mesh: TriangleMesh, point: Point[_3D]): Option[DenseVector[Float]] = {
    val normal: Vector[_3D] = mesh.normalAtPoint(point)
    val unitNormal = normal * (1.0 / normal.norm)

    val sampledPoints = featurePoints(mesh, point)
    val samples = for (samplePt <- sampledPoints) yield {
      if (image.isDefinedAt(samplePt)) {
        val gradient = Vector.fromBreezeVector[_3D](image(samplePt))
        gradient dot unitNormal
      } else {
        // fail-fast: immediately return, since the entire feature is "useless"
        return None
      }
    }

    val sum = samples.map(math.abs).sum
    val features = if (sum == 0) samples else samples.map(d => d / sum)
    Some(DenseVector(features.toArray))
  }

  def featurePoints(mesh: TriangleMesh, profilePoint: Point[_3D]): immutable.IndexedSeq[Point[_3D]] = {
    val normal: Vector[_3D] = mesh.normalAtPoint(profilePoint)
    val unitNormal = normal * (1.0 / normal.norm)
    require(math.abs(unitNormal.norm - 1.0) < 1e-5)

    val range = ((-1 * numberOfPoints / 2) to (numberOfPoints / 2)).to[immutable.IndexedSeq]
    range map { i => profilePoint + unitNormal * i * spacing }
  }
}

object NormalDirectionFeatureExtractorIOHandler extends FeatureExtractorIOHandler {
  override def identifier: String = NormalDirectionFeatureExtractor.IOIdentifier

  private val NumberOfPoints = "numberOfPoints"
  private val Spacing = "spacing"

  override def load(meta: IOMetadata, h5File: HDF5File, h5Group: Group): Try[FeatureExtractor] = {
    val groupName = h5Group.getFullName
    for {
      numberOfPoints <- h5File.readInt(s"$groupName/$NumberOfPoints")
      spacing <- h5File.readFloat(s"$groupName/$Spacing")
    } yield NormalDirectionFeatureExtractor(numberOfPoints, spacing, meta)
  }

  override def save(t: FeatureExtractor, h5File: HDF5File, h5Group: Group): Try[Unit] = {
    t match {
      case fe: NormalDirectionFeatureExtractor =>
        val groupName = h5Group.getFullName
        for {
          _ <- h5File.writeInt(s"$groupName/$NumberOfPoints", fe.numberOfPoints)
          _ <- h5File.writeFloat(s"$groupName/$Spacing", fe.spacing)
        } yield ()
      case _ => Failure(new IllegalArgumentException("unsupported feature extractor class: " + t.getClass.getName))
    }
  }
}
