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
import io.jhdf.api.Group
import scalismo.common.PointId
import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.hdf5json.HDFPath
import scalismo.io.statisticalmodel.{HDF5Reader, HDF5Writer, StatisticalModelReader}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.asm.PreprocessedImage.{Gradient, Intensity}

import scala.collection.immutable
import scala.util.{Failure, Try}

trait FeatureExtractor
    extends Function4[PreprocessedImage, Point[_3D], TriangleMesh[_3D], PointId, Option[DenseVector[Double]]]
    with HasIOMetadata {

  /**
   * Actually extracts features from an image. The image and featurePoint arguments are arguably necessary for all
   * feature extractor implementations. The additional two parameters (mesh and point id) are passed in because the
   * feature extractor might need that information. For instance, the NormalDirectionFeatureExtractor needs that
   * information to determine the "spatial orientation" of the line on which the image points to be examined have to
   * lie.
   *
   * @param image
   *   the image to extract features in
   * @param featurePoint
   *   the point in space to extract features at
   * @param mesh
   *   a mesh instance in correspondence with the image
   * @param profilePointId
   *   a point id on the mesh, corresponding to a profiled point id.
   * @return
   */
  def apply(image: PreprocessedImage,
            featurePoint: Point[_3D],
            mesh: TriangleMesh[_3D],
            profilePointId: PointId
  ): Option[DenseVector[Double]]

  /**
   * Return the points at which feature components are extracted for a given mesh and point. This is mainly intended for
   * visualization, and only really makes sense if there exists a one-to-one correspondence between such points and the
   * actual feature components returned by the <code>apply()</code>.
   *
   * In other words: if a particular implementation uses a method for determining features where there is no
   * correspondence between the feature components and particular points, the method should return [[None]]. Otherwise,
   * it should return Some(IndexedSeq(...)), where the length of the sequence matches the length of the feature vector.
   *
   * For the difference between profilePointId and featurePoint, see the documentation of the apply method.
   * @param mesh
   *   the mesh to determine the feature points for
   * @param profilePointId
   *   the profile point id to determine the feature points for
   * @param featurePoint
   *   the actual point in space where the features are to be extracted
   * @return
   *   a sequence of points, or [[None]] if there is no sensible correspondence between feature and points, as outlined
   *   above.
   */
  def featurePoints(mesh: TriangleMesh[_3D],
                    profilePointId: PointId,
                    featurePoint: Point[_3D]
  ): Option[immutable.IndexedSeq[Point[_3D]]]
}

trait FeatureExtractorIOHandler extends IOHandler[FeatureExtractor]

object FeatureExtractorIOHandlers extends IOHandlers[FeatureExtractor, FeatureExtractorIOHandler] {
  register(NormalDirectionFeatureExtractorIOHandler)
}

object NormalDirectionFeatureExtractor {
  val IOIdentifier = "builtin::NormalDirection"
  val IOMetadata_1_0 = IOMetadata(IOIdentifier, 1, 0)
  val IOMetadata_Default = IOMetadata_1_0
}

case class NormalDirectionFeatureExtractor(numberOfPoints: Int,
                                           spacing: Double,
                                           override val ioMetadata: IOMetadata =
                                             NormalDirectionFeatureExtractor.IOMetadata_Default
) extends FeatureExtractor {
  override def apply(image: PreprocessedImage,
                     point: Point[_3D],
                     mesh: TriangleMesh[_3D],
                     profilePointId: PointId
  ): Option[DenseVector[Double]] = {
    val normal: EuclideanVector[_3D] = mesh.vertexNormals(
      profilePointId
    ) // TODO: this was adapted when switched to new mesh... Check if this is correct.
    val unitNormal = normal * (1.0 / normal.norm)

    val sampledPoints =
      featurePoints(mesh, profilePointId, point).get // .get is safe: we know that the method always returns Some(...)

    val samples = for (samplePt <- sampledPoints) yield {
      if (image.isDefinedAt(samplePt)) {
        image.valueType match {
          case Intensity => image(samplePt)(0).toDouble
          case Gradient =>
            val gradient = EuclideanVector.fromBreezeVector[_3D](image(samplePt).map(_.toDouble))
            gradient dot unitNormal
        }
      } else {
        // fail-fast: immediately return, since the entire feature is "useless"
        return None
      }
    }

    val sum = samples.map(math.abs).sum
    val features = if (sum == 0 || image.valueType == Intensity) samples else samples.map(d => d / sum)
    Some(DenseVector(features.toArray))
  }

  override def featurePoints(mesh: TriangleMesh[_3D],
                             profilePointId: PointId,
                             centerPoint: Point[_3D]
  ): Option[immutable.IndexedSeq[Point[_3D]]] = {
    val normal: EuclideanVector[_3D] = mesh.vertexNormals(profilePointId)
    val unitNormal = normal * (1.0 / normal.norm)
    require(math.abs(unitNormal.norm - 1.0) < 1e-5)

    val range = ((-1 * numberOfPoints / 2) to (numberOfPoints / 2)).toIndexedSeq
    Some(range map { i =>
      centerPoint + unitNormal * i * spacing
    })
  }

}

object NormalDirectionFeatureExtractorIOHandler extends FeatureExtractorIOHandler {
  override def identifier: String = NormalDirectionFeatureExtractor.IOIdentifier

  private val NumberOfPoints = "numberOfPoints"
  private val Spacing = "spacing"

  override def load(meta: IOMetadata, modelReader: StatisticalModelReader, path: HDFPath): Try[FeatureExtractor] = {
    val groupName = path
    for {
      numberOfPoints <- modelReader.readInt(HDFPath(groupName, NumberOfPoints))
      spacing <- modelReader.readFloat(HDFPath(groupName, Spacing))
    } yield NormalDirectionFeatureExtractor(numberOfPoints, spacing.toDouble, meta)
  }

  override def save(t: FeatureExtractor, h5File: HDF5Writer, path: HDFPath): Try[Unit] = {
    t match {
      case fe: NormalDirectionFeatureExtractor =>
        for {
          _ <- h5File.writeInt(HDFPath(path, NumberOfPoints), fe.numberOfPoints)
          _ <- h5File.writeFloat(HDFPath(path, Spacing), fe.spacing.toFloat)
        } yield ()
      case _ => Failure(new IllegalArgumentException("unsupported feature extractor class: " + t.getClass.getName))
    }
  }
}
