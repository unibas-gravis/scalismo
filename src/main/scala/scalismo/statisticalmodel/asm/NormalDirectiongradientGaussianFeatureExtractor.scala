package scalismo.statisticalmodel.asm

import breeze.linalg.DenseVector
import ncsa.hdf.`object`.Group
import scalismo.geometry.{Vector, Point, _3D}
import scalismo.image.DiscreteScalarImage
import scalismo.io.HDF5File
import scalismo.mesh.TriangleMesh

import scala.util.{Failure, Try}

object NormalDirectionGradientGaussianFeatureExtractor {
  final val Identifier = "builtin::NormalDirectionGradientGaussian"
}

case class NFE(numberOfPoints: Int, spacing: Float, sigma: Float) extends FE {
  import FeatureExtractor._

  override def identifier: String = ???

  override def apply(image: DiscreteScalarImage[_3D, Float]): (TriangleMesh => FI) = {
    val gradientImage = {
      if (sigma > 0) {
        FeatureExtractor.filterGaussian(image, sigma)
      } else {
        image
      }
    }.interpolate(1).differentiate

    { mesh: TriangleMesh =>
      { point: Point[_3D] =>
        val normal: Vector[_3D] = mesh.normalAtPoint(point)
        val unitNormal = normal * (1.0 / normal.norm)
        require(math.abs(unitNormal.norm - 1.0) < 1e-5)

        val range = (-1 * numberOfPoints / 2) to (numberOfPoints / 2)
        val samples = for (i <- range) yield {
          val samplePt = point + unitNormal * i * spacing
          if (gradientImage.isDefinedAt(samplePt)) {
            gradientImage(samplePt) dot unitNormal
          } else
            999f // background value
        }

        val sum = samples.map(math.abs).sum
        val features = if (sum == 0) samples else samples.map(d => d / sum)
        DenseVector(features.toArray)
      }
    }
  }
}

case class NormalDirectionGradientGaussianFeatureExtractor(numberOfPoints: Int, spacing: Float, sigma: Float) extends FeatureExtractor {
  override def identifier = NormalDirectionGradientGaussianFeatureExtractor.Identifier

  override def apply(image: DiscreteScalarImage[_3D, Float]): FeatureImageGenerator = new FeatureImageGenerator {
    val gradientImage = {
      if (sigma > 0) {
        FeatureExtractor.filterGaussian(image, sigma)
      } else {
        image
      }
    }.interpolate(1).differentiate

    override def apply(mesh: TriangleMesh): FeatureImage = new FeatureImage {
      override def apply(point: Point[_3D]): DenseVector[Float] = {
        val normal: Vector[_3D] = mesh.normalAtPoint(point)
        val unitNormal = normal * (1.0 / normal.norm)
        require(math.abs(unitNormal.norm - 1.0) < 1e-5)

        val range = (-1 * numberOfPoints / 2) to (numberOfPoints / 2)
        val samples = for (i <- range) yield {
          val samplePt = point + unitNormal * i * spacing
          if (gradientImage.isDefinedAt(samplePt)) {
            gradientImage(samplePt) dot unitNormal
          } else
            999f // background value
        }

        val sum = samples.map(math.abs).sum
        val features = if (sum == 0) samples else samples.map(d => d / sum)
        DenseVector(features.toArray)
      }
    }
  }
}

object NormalDirectionGradientGaussianFeatureExtractorSerializer extends FeatureExtractorSerializer {
  override val identifier = NormalDirectionGradientGaussianFeatureExtractor.Identifier
  private val NumberOfPoints = "numberOfPoints"
  private val Spacing = "spacing"
  private val Sigma = "sigma"

  override def loadHdf5(h5File: HDF5File, h5Group: Group): Try[NormalDirectionGradientGaussianFeatureExtractor] = {
    val groupName = h5Group.getFullName
    for {
      numPointsForProfile <- h5File.readInt(s"$groupName/$NumberOfPoints")
      profileSpacing <- h5File.readFloat(s"$groupName/$Spacing")
      sigma <- h5File.readFloat(s"$groupName/$Sigma")
    } yield NormalDirectionGradientGaussianFeatureExtractor(numPointsForProfile, profileSpacing, sigma)
  }

  override def saveHdf5(featureExtractor: FeatureExtractor, h5File: HDF5File, h5Group: Group): Try[Unit] = {
    featureExtractor match {
      case fe@NormalDirectionGradientGaussianFeatureExtractor(numberOfPoints, spacing, sigma) =>
        val groupName = h5Group.getFullName
        for {
          _ <- h5File.writeStringAttribute(groupName, FeatureExtractorSerializer.IdentifierAttributeName, identifier)
          _ <- h5File.writeInt(s"$groupName/$NumberOfPoints", fe.numberOfPoints)
          _ <- h5File.writeFloat(s"$groupName/$Spacing", fe.spacing)
          _ <- h5File.writeFloat(s"$groupName/$Sigma", fe.sigma)
        } yield ()
      case _ => Failure(new IllegalArgumentException("unsupported feature extractor class: " + featureExtractor.getClass.getName))
    }
  }
}

