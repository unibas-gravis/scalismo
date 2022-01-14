package scalismo.sampling.parameters

import breeze.linalg.DenseVector
import scalismo.common.{DiscreteDomain, Vectorizer}
import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry.{EuclideanVector, EuclideanVector3D, Point, Point3D, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.registration.GaussianProcessTransformation3D
import scalismo.sampling.parameters.{RotationParameters, TranslationParameters}
import scalismo.sampling.{ParameterConversion, ProposalGenerator, SampleLens, TransitionProbability}
import scalismo.statisticalmodel.PointDistributionModel
import scalismo.transformations.{Rotation3D, Transformation, Transformation3D, Translation3D, TranslationAfterRotation, TranslationAfterRotation3D}



case class ShapeParameters(coefficients: DenseVector[Double])

object ShapeParameters {
  implicit object pconversion extends ParameterConversion[DenseVector[Double], ShapeParameters] {
    override def to(a: DenseVector[Double]): ShapeParameters = ShapeParameters(a)

    override def from(b: ShapeParameters): DenseVector[Double] = b.coefficients
  }
}

case class TranslationParameters(translationVector: EuclideanVector[_3D])
object TranslationParameters {
  implicit object pconversion extends ParameterConversion[(Double, Double, Double), TranslationParameters] {
    override def from(t: TranslationParameters): (Double, Double, Double) = (t.translationVector.x, t.translationVector.y, t.translationVector.z)

    override def to(t: (Double, Double, Double)): TranslationParameters = TranslationParameters(EuclideanVector3D(t._1, t._2, t._3))
  }
}

case class RotationParameters(angles: (Double, Double, Double), centerOfRotation: Point[_3D])
object RotationParameters {
    def productConversion(center : Point[_3D]) : ParameterConversion[(Double, Double, Double), RotationParameters] =
      new ParameterConversion[(Double, Double, Double), RotationParameters] {
      override def from(t: RotationParameters): (Double, Double, Double) = (t.angles._1, t.angles._2, t.angles._3)

      override def to(t: (Double, Double, Double)): RotationParameters = RotationParameters(angles = t, centerOfRotation = center)
    }

}

case class PoseAndShapeParameters(tp : TranslationParameters, rp : RotationParameters, sp : ShapeParameters)

object PoseAndShapeParameters {
  implicit object PoseAndShapeParametersConversion extends ParameterConversion[(TranslationParameters, RotationParameters, ShapeParameters), PoseAndShapeParameters] {
    override def from(t: PoseAndShapeParameters): (TranslationParameters, RotationParameters, ShapeParameters) = (t.tp, t.rp, t.sp)

    override def to(t: (TranslationParameters, RotationParameters, ShapeParameters)): PoseAndShapeParameters = PoseAndShapeParameters(t._1, t._2, t._3)
  }
}

