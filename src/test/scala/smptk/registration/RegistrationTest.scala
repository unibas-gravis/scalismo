package smptk
package registration

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

class RegistrationTest extends FunSpec with ShouldMatchers {
  smptk.initialize()
  describe("A 2D rigid landmark based registration") {
    it("can retrieve correct parameters") {
      val points : IndexedSeq[Point[TwoD]] = IndexedSeq(Point2D(0., 0.), Point2D(1., 4.), Point2D(2., 0.))

      val c = Point2D(1., 4 / 3.)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector(-angle)
        val transParams = DenseVector[Double](1f, 1.5f)
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

      val translationParams = DenseVector(1.5, 1., 3.5)
      val parameterVector = DenseVector(1.5, 1., 3.5, Math.PI, -Math.PI / 2., -Math.PI)
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
      //Utils.show2D(fixedImage,domain)
      val center = ((domain.extent - domain.origin) * 0.5).toPoint
      	
      val regConf = RegistrationConfiguration[TwoD](
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.0001, false)),
        //optimizer = LBFGSOptimizer( LBFGSOptimizerConfiguration(300)), 
        integrator = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(), 100)),
        metric = MeanSquaresMetric2D(),
        transformationSpace = TranslationSpace2D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val translationParams = DenseVector(-25., 25.)

      // val rigidTransform = RigidTransformationSpace2D(center)(DenseVector(-0f,-0f, 3.14f  / 20))
      val translationTransform = regConf.transformationSpace(translationParams)
      //val rotationTransform = RotationSpace2D(center)(DenseVector(3.14/20))
      val transformedLena = fixedImage compose translationTransform
      
      //Utils.show2D(transformedLena,domain)

      val registration = Registration.registration2D(regConf)(transformedLena, fixedImage)

      val regResult = registration(domain)

      (regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.001))
      (regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.001))
      //(regResult.parameters(0) should be (-3.14/20 plusOrMinus 0.0001))

    }
  }

  describe("A 3D image registration") {
    val testImgUrl = getClass().getResource("/chimp3D-11-DM.h5").getPath()
    val discreteFixedImage = ImageIO.read3DScalarImage[Float](new File(testImgUrl)).get
    val fixedImage = Interpolation.interpolate(discreteFixedImage, 3)

    val domain = discreteFixedImage.domain
    val origin = domain.origin
    val extent = domain.extent
    val center = ((extent - origin) * 0.5).toPoint 

    it("Recovers the correct parameters for a translation transfrom") {

      val translationParams = DenseVector(-25., 25., 50.)
      val translationTransform = TranslationSpace3D()(translationParams)
      val transformed = fixedImage compose translationTransform

      //Utils.show3D(transformed, domain)

      val regConf = RegistrationConfiguration[ThreeD](

        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.000001, true)),
        //optimizer = LBFGSOptimizer( LBFGSOptimizerConfiguration(300)), 
        integrator = Integrator[ThreeD](IntegratorConfiguration(UniformDistributionRandomSampler3D(), 100)),
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

    it("Recovers the correct parameters for a SMALL rotation transform") {
      val pi = Math.PI
      val rotationParams = DenseVector(-pi /6., pi / 5., pi / 5.)
      val rotationTransform = RotationSpace3D(center)(rotationParams)
      val transformed = fixedImage.compose(rotationTransform)

      val regConf = RegistrationConfiguration[ThreeD](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.00000001, true)),
        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = Integrator(IntegratorConfiguration(UniformDistributionRandomSampler3D(),1000)),
        metric = MeanSquaresMetric3D(),
        transformationSpace = RotationSpace3D(center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val registration = Registration.registration3D(regConf)(transformed, fixedImage)

      val regResult = registration(domain)

      val RegTransformed = fixedImage.compose(regResult.transform)

      // here we verify that the angles give similar rotation matrices 

      def computeRotMatrix(p: DenseVector[Double]) = {
        val cospsi = Math.cos(p(2))
        val sinpsi = Math.sin(p(2))

        val costh = Math.cos(p(1))
        val sinth = Math.sin(p(1))

        val cosphi = Math.cos(p(0))
        val sinphi = Math.sin(p(0))

        DenseMatrix(
          (costh * cosphi, sinpsi * sinth * cosphi - cospsi * sinphi, sinpsi * sinphi + cospsi * sinth * cosphi),
          (costh * sinphi, cospsi * cosphi + sinpsi * sinth * sinphi, cospsi * sinth * sinphi - sinpsi * cosphi),
          (-sinth, sinpsi * costh, cospsi * costh))
      }
      
      val rotMat1 = computeRotMatrix(rotationParams)
      val rotMat2 = computeRotMatrix(regResult.parameters)
      
      for(i<- 0 until 3 ; j <- 0 until 3 )
        rotMat1(i,j) should be(rotMat2(i,j) plusOrMinus 0.001)
      
    }

  }

}
