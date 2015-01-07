package org.statismo.stk.core
package registration

import scala.language.implicitConversions
import org.scalatest.FunSpec
import java.io.File
import geometry._
import breeze.linalg.DenseVector
import org.scalatest.matchers.ShouldMatchers
import org.statismo.stk.core.io.ImageIO
import org.statismo.stk.core.image.{DiscreteScalarImage}
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.{UniformSampler}
import org.statismo.stk.core.io.MeshIO
import breeze.linalg.DenseMatrix
import org.statismo.stk.core.numerics.LBFGSOptimizer
import org.statismo.stk.core.numerics.LBFGSOptimizerConfiguration
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.GradientDescentConfiguration

class RegistrationTests extends FunSpec with ShouldMatchers {

  implicit def doubleToFloat(d: Double) = d.toFloat

  org.statismo.stk.core.initialize()
  describe("A 2D rigid landmark based registration") {
    it("can retrieve correct parameters") {
      val points: IndexedSeq[Point[_2D]] = IndexedSeq(Point(0.0, 0.0), Point(1.0, 4.0), Point(2.0, 0.0))

      val c = Point(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector[Float](-angle)
        val transParams = DenseVector[Float](1f, 1.5f)
        val productParams = DenseVector.vertcat(transParams, rotationParams)

        val productSpace = RigidTransformationSpace[_2D](c)

        val transformedPoints = points.map((pt: Point[_2D]) => productSpace.transformForParameters(productParams)(pt))

        val regResult = LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints))

        val alignedPoints = points.map((pt: Point[_2D]) => regResult.transform(pt))

        transformedPoints(0)(0) should be(alignedPoints(0)(0) plusOrMinus 0.0001)
        transformedPoints(0)(1) should be(alignedPoints(0)(1) plusOrMinus 0.0001)
        transformedPoints(1)(0) should be(alignedPoints(1)(0) plusOrMinus 0.0001)
        transformedPoints(1)(1) should be(alignedPoints(1)(1) plusOrMinus 0.0001)
        transformedPoints(2)(0) should be(alignedPoints(2)(0) plusOrMinus 0.0001)
        transformedPoints(2)(1) should be(alignedPoints(2)(1) plusOrMinus 0.0001)
      }
    }
  }

  describe("A 3D rigid landmark based registration") {

    val path = getClass.getResource("/facemesh.h5").getPath
    val mesh = MeshIO.readHDF5(new File(path)).get

    val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
    val trans = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)

    val rigidTransformed = mesh warp trans

    val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.points.zip(rigidTransformed.points).toIndexedSeq)

    //should not test on parameters here since many euler angles can lead to the same rotation matrix
    val rigidRegTransformed = mesh warp regResult.transform
    it("can retrieve correct parameters") {

      for ((p, i) <- rigidRegTransformed.points.zipWithIndex) {
        p(0) should be(rigidTransformed.points(i)(0) plusOrMinus 0.0001)
        p(1) should be(rigidTransformed.points(i)(1) plusOrMinus 0.0001)
        p(2) should be(rigidTransformed.points(i)(2) plusOrMinus 0.0001)
      }
    }

    it("Rigid Transformation forth and back of a mesh gives the same points ") {
      val inverseTrans = regResult.transform.asInstanceOf[RigidTransformation[_3D]].inverse
      val tranformed = mesh.warp(regResult.transform).warp(inverseTrans)

      for ((p, i) <- tranformed.points.zipWithIndex) {
        p(0) should be(mesh.points(i)(0) plusOrMinus 0.0001)
        p(1) should be(mesh.points(i)(1) plusOrMinus 0.0001)
        p(2) should be(mesh.points(i)(2) plusOrMinus 0.0001)
      }
    }
  }

  describe("A 2D similarity landmark based registration") {
    it("can transform the points appropriately") {
      val points: IndexedSeq[Point[_2D]] = IndexedSeq(Point(0.0, 0.0), Point(1.0, 4.0), Point(2.0, 0.0))

      val c = Point(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector[Float](-angle)
        val transParams = DenseVector[Float](1f, 1.5f)

        val scalingFactor = scala.util.Random.nextFloat()
        val productParams = DenseVector.vertcat(DenseVector.vertcat(transParams, rotationParams), DenseVector(scalingFactor))

        val productSpace = RigidTransformationSpace[_2D](c).product(ScalingSpace[_2D])

        val transformedPoints = points.map((pt: Point[_2D]) => productSpace.transformForParameters(productParams)(pt))

        val regResult = LandmarkRegistration.similarity2DLandmarkRegistration(points.zip(transformedPoints))

        val alignedPoints = points.map(regResult.transform)
        transformedPoints(0)(0) should be(alignedPoints(0)(0) plusOrMinus 0.0001)
        transformedPoints(0)(1) should be(alignedPoints(0)(1) plusOrMinus 0.0001)
        transformedPoints(1)(0) should be(alignedPoints(1)(0) plusOrMinus 0.0001)
        transformedPoints(1)(1) should be(alignedPoints(1)(1) plusOrMinus 0.0001)
        transformedPoints(2)(0) should be(alignedPoints(2)(0) plusOrMinus 0.0001)
        transformedPoints(2)(1) should be(alignedPoints(2)(1) plusOrMinus 0.0001)
      }
    }
  }

  describe("A 3D similarity landmark based registration") {
    it("can transform the mesh appropriately") {

      val path = getClass.getResource("/facemesh.h5").getPath
      val mesh = MeshIO.readHDF5(new File(path)).get

      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI, 2f)
      val trans = RigidTransformationSpace[_3D]().product(ScalingSpace[_3D]).transformForParameters(parameterVector)

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
      val testImgUrl = getClass.getResource("/dm128.h5").getPath
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = DiscreteScalarImage.interpolate(discreteFixedImage, 3)

      val domain = discreteFixedImage.domain

      val integr = Integrator[_2D](IntegratorConfiguration(UniformSampler(domain, 4000)))
      val regConf = RegistrationConfiguration[_2D](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.0000001, false)),
        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = integr,
        metric = MeanSquaresMetric2D(integr),
        transformationSpace = TranslationSpace[_2D],
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val translationParams = DenseVector[Float](-10.0, 5.0)
      val translationTransform = regConf.transformationSpace.transformForParameters(translationParams)
      val transformedLena = fixedImage compose translationTransform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.01)
      regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.01)
    }

    it("Recovers the correct parameters for a rotation transfrom") {
      val testImgUrl = getClass.getResource("/dm128.h5").getPath
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = DiscreteScalarImage.interpolate(discreteFixedImage, 3)

      val domain = discreteFixedImage.domain
      val center = ((domain.extent - domain.origin) * 0.5).toPoint

      val integr = Integrator[_2D](IntegratorConfiguration(UniformSampler(domain, 4000)))
      val regConf = RegistrationConfiguration[_2D](
        //optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(300, 1e-4)),
        integrator = integr,
        metric = MeanSquaresMetric2D(integr),
        transformationSpace = RotationSpace[_2D](center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val rotationParams = DenseVector[Float](math.Pi / 8.0)
      val transform = regConf.transformationSpace.transformForParameters(rotationParams)
      val transformedLena = fixedImage compose transform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      regResult.parameters(0) should be(rotationParams(0) plusOrMinus 0.01)
    }
  }

  describe("A 3D image registration") {
    val testImgUrl = getClass.getResource("/3ddm.h5").getPath
    val discreteFixedImage = ImageIO.read3DScalarImage[Float](new File(testImgUrl)).get
    val fixedImage = DiscreteScalarImage.interpolate(discreteFixedImage, 3)

    val domain = discreteFixedImage.domain
    val origin = domain.origin
    val extent = domain.extent
    val center = ((extent - origin) * 0.5).toPoint

    it("Recovers the correct parameters for a translation transfrom") {

      val translationParams = DenseVector[Float](-10.0, 0, 0)
      val translationTransform = TranslationSpace[_3D].transformForParameters(translationParams)
      val transformed = fixedImage compose translationTransform

      val integr = Integrator[_3D](IntegratorConfiguration(UniformSampler(domain, 20000)))
      val regConf = RegistrationConfiguration[_3D](

        optimizer = LBFGSOptimizer(LBFGSOptimizerConfiguration(300)),
        integrator = integr,
        metric = MeanSquaresMetric3D(integr),
        transformationSpace = TranslationSpace[_3D],
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val regResult = Registration.registration(regConf)(transformed, fixedImage)
      regResult.parameters(0) should be(translationParams(0) plusOrMinus 0.01)
      regResult.parameters(1) should be(translationParams(1) plusOrMinus 0.01)
      regResult.parameters(2) should be(translationParams(2) plusOrMinus 0.01)
    }

    ignore("Recovers the correct parameters for a SMALL rotation transform") {
      val pi = Math.PI
      val rotationParams = DenseVector[Float](-pi / 10, 0, 0)
      val rotationTransform = RotationSpace[_3D](center).transformForParameters(rotationParams)
      val transformed = fixedImage.compose(rotationTransform)

      val integr = Integrator(IntegratorConfiguration(UniformSampler(domain, 10000)))
      val regConf = RegistrationConfiguration[_3D](
        optimizer = GradientDescentOptimizer(GradientDescentConfiguration(400, 2e-12)),
        integrator = integr,
        metric = MeanSquaresMetric3D(integr),
        transformationSpace = RotationSpace[_3D](center),
        regularizer = RKHSNormRegularizer,
        regularizationWeight = 0.0)

      val regResult = Registration.registration(regConf)(transformed, fixedImage)

      val regParams: DenseVector[Float] = regResult.parameters
      for (i <- 0 until rotationParams.size) {
        regParams(i) should be(rotationParams(i) plusOrMinus 0.01)
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
