package smptk
package registration

import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import smptk.image.DiscreteImageDomain2D
import smptk.image.Geometry.CoordVector2D
import breeze.linalg.DenseVector
import org.scalatest.matchers.ShouldMatchers
import smptk.image.Geometry._
import smptk.image.Geometry.implicits._
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

class RegistrationTest extends FunSpec with ShouldMatchers {
  describe("A 2D rigid landmark based registration") {
    ignore("can retrieve correct parameters") {
      val points = IndexedSeq(CoordVector2D(0., 0.), CoordVector2D(1., 4.), CoordVector2D(2., 0.))

      val c = CoordVector2D(1., 4 / 3.)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector(-angle)
        val transParams = DenseVector[Double](1f, 1.5f)
        val productParams = DenseVector.vertcat(transParams, rotationParams)

        val productSpace = RigidTransformationSpace2D(c)

        val transformedPoints = points.map((pt: CoordVector2D[Double]) => productSpace(productParams)(pt))

        val regResult = LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints), c)

        val alignedPoints = points.map((pt: CoordVector2D[Double]) => regResult.transform(pt))

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
    it("can retrieve correct parameters for rotation only") {

      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get

      val region = mesh.boundingBox
      val origin = region.origin
      val extent = region.extent
      val center = CoordVector3D[Double]((extent(0) - origin(0)) / 2., (extent(1) - origin(1)) / 2., (extent(2) - origin(2)) / 2.)

      val translationParams = DenseVector(1.5, 1., 3.5)
      val parameterVector = DenseVector(1.5, 1., 3.5,  0.5, 0.9, 0.9)
      val trans = RigidTransformationSpace3D(center)(parameterVector)

      val rotatedTrans = mesh compose trans
      Utils.showVTK(Utils.meshToVTKMesh(rotatedTrans))

      val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.domain.points.zip(rotatedTrans.domain.points).toIndexedSeq, center)

      println("Result of transform " + regResult.parameters)
      println("Should be " + parameterVector)
      Utils.showVTK(Utils.meshToVTKMesh(mesh compose regResult.transform))

      for (d <- 0 until parameterVector.size)
        regResult.parameters(d) should be(parameterVector(d) plusOrMinus 0.0001)

    }
  }

  describe("A 2D image registration") {
    ignore("Recovers the correct parameters for a translation transfrom") {
      val testImgUrl = getClass().getResource("/dm128.h5").getPath()
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = Interpolation.interpolate2D(3)(discreteFixedImage)

      val domain = discreteFixedImage.domain
      val center = CoordVector2D(domain.origin(0) + domain.extent(0) / 2, domain.origin(1) + domain.extent(1) / 2)

      val regConf = RegistrationConfiguration[CoordVector2D](
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.0001, true)),
        //optimizer = LBFGSOptimizer( LBFGSOptimizerConfiguration(300)), 
        integrator = Integrator[CoordVector2D](IntegratorConfiguration(UniformDistributionRandomSampler2D(100))),
        metric = MeanSquaresMetric2D(),
        transformationSpace = TranslationSpace2D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val translationParams = DenseVector(-25., 25.)

      // val rigidTransform = RigidTransformationSpace2D(center)(DenseVector(-0f,-0f, 3.14f  / 20))
      val translationTransform = regConf.transformationSpace(translationParams)
      //val rotationTransform = RotationSpace2D(center)(DenseVector(3.14/20))
      val transformedLena = fixedImage compose translationTransform

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
    val fixedImage = Interpolation.interpolate3D(3)(discreteFixedImage)

    val domain = discreteFixedImage.domain
    val origin = domain.origin
    val extent = domain.extent
    val center = CoordVector3D[Double]((extent(0) - origin(0)) / 2., (extent(1) - origin(1)) / 2., (extent(2) - origin(2)) / 2.)

    ignore("Recovers the correct parameters for a translation transfrom") {

      val translationParams = DenseVector(-25., 25., 50.)
      val translationTransform = TranslationSpace3D()(translationParams)
      val transformed = fixedImage compose translationTransform

      //Utils.show3D(transformed, domain)

      val regConf = RegistrationConfiguration[CoordVector3D](
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.000001, true)),
        //optimizer = LBFGSOptimizer( LBFGSOptimizerConfiguration(300)), 
        integrator = Integrator[CoordVector3D](IntegratorConfiguration(UniformDistributionRandomSampler3D(100))),
        metric = MeanSquaresMetric3D(),
        transformationSpace = TranslationSpace3D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val registration = Registration.registration3D(regConf)(transformed, fixedImage)

      val regResult = registration(domain)

      (regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.001))
      (regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.001))
      (regResult.parameters(2) should be(translationParams(2) plusOrMinus 0.001))
    }

    //    it("Recovers the correct parameters for a rotation transform") {
    //      val rotationParams = DenseVector(-0.07, 0.5, 0.03)
    //      val rotationTransform = RotationSpace3D(center)(rotationParams)
    //      val transformed = fixedImage compose rotationTransform
    //      
    //      
    //      
    //      
    //      
    //       def pyramidRegistation(fixedImage: ContinuousScalarImage3D, movingImage: ContinuousScalarImage3D,
    //        domain: DiscreteImageDomain3D,
    //        regConf: RegistrationConfiguration[CoordVector2D],
    //        deviations: List[Double],
    //        latestRegResults: Option[RegistrationResult[CoordVector2D]]): RegistrationResult[CoordVector2D] = {
    //  
    //        if (deviations.size > 0)
    //        	println("Pyramid registration for deviation " + deviations(0))
    //
    //        val integrator = Integrator[CoordVector3D](IntegratorConfiguration(UniformSampler3D(50)))
    //        val image = if (deviations.size == 0) movingImage else Utils.gaussianSmoothing3D(movingImage, deviations(0), integrator)
    //        val smoothedFixedImage = if (deviations.size == 0) fixedImage else Utils.gaussianSmoothing2D(fixedImage, deviations(0), integrator)
    //        
    //        val lastParams = if (latestRegResults.isDefined) latestRegResults.get.parameters else regConf.initialParameters
    //
    //        val newRegConf = RegistrationConfiguration[CoordVector2D](
    //          regularizationWeight = regConf.regularizationWeight,
    //          optimizer = regConf.optimizer,
    //          integrator = regConf.integrator,
    //          metric = regConf.metric,
    //          transformationSpace = regConf.transformationSpace,
    //          regularizer = RKHSNormRegularizer,
    //          initialParametersOrNone = Some(lastParams))
    //
    //      
    //      
    //      
    //      
    //      
    //      
    //      val regConf = RegistrationConfiguration[CoordVector3D](
    //          
    //        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(100, 0.00000001, false)),
    //        //optimizer = LBFGSOptimizer( LBFGSOptimizerConfiguration(300)), 
    //        integrator = Integrator[CoordVector3D](IntegratorConfiguration(UniformDistributionRandomSampler3D(100))),
    //        metric = MeanSquaresMetric3D(),
    //        transformationSpace = RotationSpace3D(center),
    //        regularizer = RKHSNormRegularizer,
    //        regularizationWeight = 0.0)
    //
    //      val registration = Registration.registration3D(regConf)(transformed, fixedImage)
    //
    //      val regResult = registration(domain)
    //
    //      (regResult.parameters(0) should be(rotationParams(0) plusOrMinus 0.001))
    //      (regResult.parameters(1) should be(rotationParams(1) plusOrMinus 0.001))
    //      (regResult.parameters(2) should be(rotationParams(2) plusOrMinus 0.001))
    //      
    //
    //    }
    //
  }

}
