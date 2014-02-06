package org.statismo.stk.core
package registration

import scala.language.implicitConversions
import org.scalatest.FunSpec
import java.nio.ByteBuffer
import java.io.File
import java.io.IOException
import org.statismo.stk.core.image.DiscreteImageDomain2D
import geometry._
import geometry.implicits._
import breeze.linalg.DenseVector
import org.scalatest.matchers.ShouldMatchers
import breeze.plot.Figure
import breeze.plot._
import org.statismo.stk.core.io.ImageIO
import org.statismo.stk.core.image.Interpolation
import org.statismo.stk.core.image.{ DiscreteImageDomain1D, ContinuousScalarImage3D, DiscreteImageDomain3D }
import org.statismo.stk.core.image.DiscreteScalarImage1D
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.GradientDescentConfiguration
import org.statismo.stk.core.numerics.LBFGSOptimizer
import org.statismo.stk.core.numerics.LBFGSOptimizerConfiguration
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.{ UniformDistributionRandomSampler2D, UniformDistributionRandomSampler3D }
import org.statismo.stk.core.image.Utils
import org.statismo.stk.core.numerics.UniformSampler3D
import org.statismo.stk.core.io.MeshIO
import breeze.linalg.DenseMatrix
import org.statismo.stk.core.numerics.GradientDescentConfiguration
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.LBFGSOptimizer
import org.statismo.stk.core.numerics.LBFGSOptimizerConfiguration
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.GradientDescentConfiguration
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.GradientDescentConfiguration
import org.statismo.stk.core.utils.Visualization._

class RegistrationTest extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  org.statismo.stk.core.initialize()
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

        val regResult = LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints))

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

      val rotated = mesh warp trans
      //Utils.showVTK(Utils.meshToVTKMesh(rotatedTrans))

      val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.points.zip(rotated.points).toIndexedSeq)
      //Utils.showVTK(Utils.meshToVTKMesh(mesh compose regResult.transform))

      //should not test on parameters here since many euler angles can lead to the same rotation matrix
      val regRotated = mesh warp regResult.transform

      for ((p, i) <- regRotated.points.zipWithIndex) {
        p(0) should be(rotated.points(i)(0) plusOrMinus 0.0001)
        p(1) should be(rotated.points(i)(1) plusOrMinus 0.0001)
        p(2) should be(rotated.points(i)(2) plusOrMinus 0.0001)
      }
    }
  }
  
  describe("A 2D similarity landmark based registration") {
    it("can transform the points appropriately") {
      val points: IndexedSeq[Point[TwoD]] = IndexedSeq(Point2D(0.0, 0.0), Point2D(1.0, 4.0), Point2D(2.0, 0.0))

      val c = Point2D(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector[Float](-angle)
        val transParams = DenseVector[Float](1f, 1.5f)
        
        val scalingFactor = scala.util.Random.nextFloat
        val productParams = DenseVector.vertcat(DenseVector.vertcat(transParams, rotationParams), DenseVector(scalingFactor))

        val productSpace = RigidTransformationSpace2D(c).product(ScalingSpace2D())

        val transformedPoints = points.map((pt: Point[TwoD]) => productSpace(productParams)(pt))

        val regResult = LandmarkRegistration.similarity2DLandmarkRegistration(points.zip(transformedPoints))

        val alignedPoints = points.map(regResult.transform)
        (transformedPoints(0)(0) should be(alignedPoints(0)(0) plusOrMinus 0.0001))
        (transformedPoints(0)(1) should be(alignedPoints(0)(1) plusOrMinus 0.0001))
        (transformedPoints(1)(0) should be(alignedPoints(1)(0) plusOrMinus 0.0001))
        (transformedPoints(1)(1) should be(alignedPoints(1)(1) plusOrMinus 0.0001))
        (transformedPoints(2)(0) should be(alignedPoints(2)(0) plusOrMinus 0.0001))
        (transformedPoints(2)(1) should be(alignedPoints(2)(1) plusOrMinus 0.0001))
      }
    }
  }

  describe("A 3D similarity landmark based registration") {
    it("can transform the mesh appropriately") {

      val path = getClass().getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get

      val region = mesh.boundingBox
      val origin = region.origin
      val extent = region.extent
      val center = ((extent - origin) * 0.5).toPoint

      val translationParams = DenseVector[Float](1.5, 1.0, 3.5)
      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI, 2f)
      val trans = RigidTransformationSpace3D(center).product(ScalingSpace3D())(parameterVector)

      val translatedRotatedScaled = mesh warp trans
    
      val regResult = LandmarkRegistration.similarity3DLandmarkRegistration(mesh.points.zip(translatedRotatedScaled.points).toIndexedSeq)

      //should not test on parameters here since many euler angles can lead to the same rotation matrix
      val regSim = mesh warp regResult.transform
      
      for ((p, i) <- regSim.points.zipWithIndex.take(100)) {
        p(0) should be(translatedRotatedScaled.points(i)(0) plusOrMinus 0.0001)
        p(1) should be(translatedRotatedScaled.points(i)(1) plusOrMinus 0.0001)
        p(2) should be(translatedRotatedScaled.points(i)(2) plusOrMinus 0.0001)
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

      val integr = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(domain, 4000)))
      val regConf = RegistrationConfiguration[TwoD](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.0000001, false)),
        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = integr,
        metric = MeanSquaresMetric2D(integr),
        transformationSpace = TranslationSpace2D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val translationParams = DenseVector[Float](-10.0, 5.0)
      val translationTransform = regConf.transformationSpace(translationParams)
      val transformedLena = fixedImage compose translationTransform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      (regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.01))
      (regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.01))
    }

    it("Recovers the correct parameters for a rotation transfrom") {
      val testImgUrl = getClass().getResource("/dm128.h5").getPath()
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = Interpolation.interpolate(discreteFixedImage, 3)

      val domain = discreteFixedImage.domain
      val center = ((domain.extent - domain.origin) * 0.5).toPoint

      val integr = Integrator[TwoD](IntegratorConfiguration(UniformDistributionRandomSampler2D(domain, 4000)))
      val regConf = RegistrationConfiguration[TwoD](
        //optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(300, 1e-4)),
        integrator = integr,
        metric = MeanSquaresMetric2D(integr),
        transformationSpace = RotationSpace2D(center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val rotationParams = DenseVector[Float](math.Pi / 8.0)
      val transform = regConf.transformationSpace(rotationParams)
      val transformedLena = fixedImage compose transform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

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

      val integr = Integrator[ThreeD](IntegratorConfiguration(UniformDistributionRandomSampler3D(domain, 20000)))
      val regConf = RegistrationConfiguration[ThreeD](

        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = integr,
        metric = MeanSquaresMetric3D(integr),
        transformationSpace = TranslationSpace3D(),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val regResult = Registration.registration(regConf)(transformed, fixedImage)
      (regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.01))
      (regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.01))
      (regResult.parameters(2) should be(translationParams(2) plusOrMinus 0.01))
    }

    ignore("Recovers the correct parameters for a SMALL rotation transform") {
      val pi = Math.PI
      val rotationParams = DenseVector[Float](-pi / 10, 0, 0)
      val rotationTransform = RotationSpace3D(center)(rotationParams)
      val transformed = fixedImage.compose(rotationTransform)

      val integr = Integrator(IntegratorConfiguration(UniformDistributionRandomSampler3D(domain, 10000)))
      val regConf = RegistrationConfiguration[ThreeD](
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(400, 2e-12)),
        integrator = integr,
        metric = MeanSquaresMetric3D(integr),
        transformationSpace = RotationSpace3D(center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val regResult = Registration.registration(regConf)(transformed, fixedImage)

      val RegTransformed = fixedImage.compose(regResult.transform)

      val regParams: DenseVector[Float] = regResult.parameters
      for (i <- 0 until rotationParams.size) {
        regParams(i) should be(rotationParams(i) plusOrMinus (0.01))
      }

      // here we verify that the angles give similar rotation matrices 

      def computeRotMatrix(p: DenseVector[Float]): DenseMatrix[Float] = {
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
