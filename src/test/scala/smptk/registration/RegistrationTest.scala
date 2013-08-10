package smptk
package registration

import scala.language.implicitConversions
import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import smptk.image.DiscreteImageDomain2D
import geometry._
import geometry.implicits._
import breeze.linalg.DenseVector
import org.scalatest.matchers.ShouldMatchers
import breeze.plot.Figure
import breeze.plot._
import smptk.io.ImageIO
import smptk.image.Interpolation
import smptk.image.Image._
import smptk.image.{ DiscreteImageDomain1D, ContinuousScalarImage3D, DiscreteImageDomain3D }
import smptk.image.DiscreteScalarImage1D
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.GradientDescentConfiguration
import smptk.numerics.LBFGSOptimizer
import smptk.numerics.LBFGSOptimizerConfiguration
import smptk.numerics.Integrator
import smptk.numerics.IntegratorConfiguration
import smptk.numerics.{ UniformDistributionRandomSampler2D, UniformDistributionRandomSampler3D }
import smptk.image.Utils
import smptk.numerics.UniformSampler3D
import smptk.io.MeshIO
import breeze.linalg.DenseMatrix
import smptk.numerics.GradientDescentConfiguration
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.LBFGSOptimizer
import smptk.numerics.LBFGSOptimizerConfiguration
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.GradientDescentConfiguration
import smptk.numerics.GradientDescentOptimizer
import smptk.numerics.GradientDescentConfiguration

class RegistrationTest extends FunSpec with ShouldMatchers {
  
  implicit def doubleToFloat(d : Double) =d.toFloat
  
  smptk.initialize()
  describe("A 2D rigid landmark based registration") {
    it("can retrieve correct parameters") {
      val points: IndexedSeq[Point[TwoD]] = IndexedSeq(Point2D(0.0, 0.0), Point2D(1.0, 4.0), Point2D(2.0, 0.0))

      val c = Point2D(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector[Float](-angle)
        val transParams = DenseVector[Float](1f, 1.5f)
        val productParams = DenseVector.vertcat(transParams, rotationParams)

        val productSpace = RigidTransformationSpace2D(c)

        val transformedPoints = points.map((pt: Point[TwoD]) => productSpace(productParams)(pt))

        val regResult = LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints), c)

        val alignedPoints = points.map((pt: Point[TwoD]) => regResult.transform(pt))

        (transformedPoints(0)(0) should be(alignedPoints(0)(0) plusOrMinus 0.0001))
        (transformedPoints(0)(1) should be(alignedPoints(0)(1) plusOrMinus 0.0001))
        (transformedPoints(1)(0) should be(alignedPoints(1)(0) plusOrMinus 0.0001))
        (transformedPoints(1)(1) should be(alignedPoints(1)(1) plusOrMinus 0.0001))
        (transformedPoints(2)(0) should be(alignedPoints(2)(0) plusOrMinus 0.0001))
        (transformedPoints(2)(1) should be(alignedPoints(2)(1) plusOrMinus 0.0001))
      }
    }

  }

  describe("A 3D rigid landmark based registration") {
    it("can retrieve correct parameters") {

      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get

      val region = mesh.boundingBox
      val origin = region.origin
      val extent = region.extent
      val center = ((extent - origin) * 0.5).toPoint

      val translationParams = DenseVector[Float](1.5, 1.0, 3.5)
      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
      val trans = RigidTransformationSpace3D(center)(parameterVector)

      val rotated = mesh compose trans
      //Utils.showVTK(Utils.meshToVTKMesh(rotatedTrans))

      val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.points.zip(rotated.points).toIndexedSeq, center)
      //Utils.showVTK(Utils.meshToVTKMesh(mesh compose regResult.transform))

      //should not test on parameters here since many euler angles can lead to the same rotation matrix
      val regRotated = mesh compose regResult.transform

      for ((p, i) <- regRotated.points.zipWithIndex) {
        p(0) should be(rotated.points(i)(0) plusOrMinus 0.0001)
        p(1) should be(rotated.points(i)(1) plusOrMinus 0.0001)
        p(2) should be(rotated.points(i)(2) plusOrMinus 0.0001)
      }
    }
  }

  describe("A 2D image registration") {
    it("Recovers the correct parameters for a translation transfrom") {
      val testImgUrl = getClass().getResource("/dm128.h5").getPath()
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = Interpolation.interpolate(discreteFixedImage, 3)

      val domain = discreteFixedImage.domain
      val center = ((domain.extent - domain.origin) * 0.5).toPoint

      val regConf = RegistrationConfiguration[TwoD](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.0000001, false)),
        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(domain), 4000)),
        metric = MeanSquaresMetric2D(),
        transformationSpace = TranslationSpace2D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val translationParams = DenseVector[Float](-10.0, 5.0)
      val translationTransform = regConf.transformationSpace(translationParams)
      val transformedLena = fixedImage compose translationTransform
      val registration = Registration.registration2D(regConf)(transformedLena, fixedImage)

      val regResult = registration(domain)

      (regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.01))
      (regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.01))
    }

    it("Recovers the correct parameters for a rotation transfrom") {
      val testImgUrl = getClass().getResource("/dm128.h5").getPath()
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = Interpolation.interpolate(discreteFixedImage, 3)

      val domain = discreteFixedImage.domain
      val center = ((domain.extent - domain.origin) * 0.5).toPoint

      val regConf = RegistrationConfiguration[TwoD](
        //optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(300, 1e-8)),
        integrator = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(domain), 4000)),
        metric = MeanSquaresMetric2D(),
        transformationSpace = RotationSpace2D(center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val rotationParams = DenseVector[Float](math.Pi/8.0)
      val transform = regConf.transformationSpace(rotationParams)
      val transformedLena = fixedImage compose transform
      val registration = Registration.registration2D(regConf)(transformedLena, fixedImage)

      val regResult = registration(domain)

      (regResult.parameters(0) should be(rotationParams(0) plusOrMinus 0.01))
    }

  }

  describe("A 3D image registration") {
    val testImgUrl = getClass().getResource("/3ddm.h5").getPath()
    val discreteFixedImage = ImageIO.read3DScalarImage[Float](new File(testImgUrl)).get
    val fixedImage = Interpolation.interpolate(discreteFixedImage, 3)

    val domain = discreteFixedImage.domain
    val origin = domain.origin
    val extent = domain.extent
    val center = ((extent - origin) * 0.5).toPoint

    it("Recovers the correct parameters for a translation transfrom") {

      val translationParams = DenseVector[Float](-10.0, 0, 0)
      val translationTransform = TranslationSpace3D()(translationParams)
      val transformed = fixedImage compose translationTransform

      val regConf = RegistrationConfiguration[ThreeD](

        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = Integrator[ThreeD](IntegratorConfiguration(UniformDistributionRandomSampler3D(domain), 20000)),
        metric = MeanSquaresMetric3D(),
        transformationSpace = TranslationSpace3D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val registration = Registration.registration3D(regConf)(transformed, fixedImage)
      val regResult = registration(domain)
      (regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.01))
      (regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.01))
      (regResult.parameters(2) should be(translationParams(2) plusOrMinus 0.01))
    }

    ignore("Recovers the correct parameters for a SMALL rotation transform") {
      val pi = Math.PI
      val rotationParams = DenseVector[Float](-pi / 10, 0, 0)
      val rotationTransform = RotationSpace3D(center)(rotationParams)
      val transformed = fixedImage.compose(rotationTransform)

      val regConf = RegistrationConfiguration[ThreeD](
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(400, 2e-12)),
        integrator = Integrator(IntegratorConfiguration(UniformDistributionRandomSampler3D(domain), 10000)),
        metric = MeanSquaresMetric3D(),
        transformationSpace = RotationSpace3D(center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val registration = Registration.registration3D(regConf)(transformed, fixedImage)

      val regResult = registration(domain)

      val RegTransformed = fixedImage.compose(regResult.transform)

      val regParams: DenseVector[Float] = regResult.parameters
      for (i <- 0 until rotationParams.size) {
        regParams(i) should be(rotationParams(i) plusOrMinus(0.01))
      }

      // here we verify that the angles give similar rotation matrices 

      def computeRotMatrix(p: DenseVector[Float]) : DenseMatrix[Float] = {
        val cospsi = Math.cos(p(2))
        val sinpsi = Math.sin(p(2))

        val costh = Math.cos(p(1))
        val sinth = Math.sin(p(1))

        val cosphi = Math.cos(p(0))
        val sinphi = Math.sin(p(0))

        DenseMatrix(
          (costh * cosphi, sinpsi * sinth * cosphi - cospsi * sinphi, sinpsi * sinphi + cospsi * sinth * cosphi),
          (costh * sinphi, cospsi * cosphi + sinpsi * sinth * sinphi, cospsi * sinth * sinphi - sinpsi * cosphi),
          (-sinth, sinpsi * costh, cospsi * costh)).map(_.toFloat)
      }

      val rotMat1 = computeRotMatrix(rotationParams)
      val rotMat2 = computeRotMatrix(regResult.parameters)

      for (i <- 0 until 3; j <- 0 until 3)
        rotMat1(i, j) should be(rotMat2(i, j) plusOrMinus 0.001)

    }

  }

}
