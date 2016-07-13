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
package scalismo.registration

import java.io.File

import breeze.linalg.DenseVector
import scalismo.ScalismoTestSuite
import scalismo.common.{ Field, PointId, RealSpace, VectorField }
import scalismo.geometry._
import scalismo.io.{ ImageIO, MeshIO }
import scalismo.kernels.{ DiagonalKernel, GaussianKernel }
import scalismo.numerics.{ GradientDescentOptimizer, LBFGSOptimizer, UniformSampler }
import scalismo.statisticalmodel.{ GaussianProcess, LowRankGaussianProcess }

import scala.language.implicitConversions

class RegistrationTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A 2D rigid landmark based registration") {
    it("can retrieve correct parameters") {
      val points: IndexedSeq[Point[_2D]] = IndexedSeq(Point(0.0, 0.0), Point(1.0, 4.0), Point(2.0, 0.0))

      val c = Point(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector[Double](-angle)
        val transParams = DenseVector[Double](1.0, 1.5)
        val productParams = DenseVector.vertcat(transParams, rotationParams)

        val productSpace = RigidTransformationSpace[_2D](c)

        val transformedPoints = points.map((pt: Point[_2D]) => productSpace.transformForParameters(productParams)(pt))

        val regResult = LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints))

        val alignedPoints = points.map((pt: Point[_2D]) => regResult(pt))

        transformedPoints(0)(0) should be(alignedPoints(0)(0) +- 0.0001)
        transformedPoints(0)(1) should be(alignedPoints(0)(1) +- 0.0001)
        transformedPoints(1)(0) should be(alignedPoints(1)(0) +- 0.0001)
        transformedPoints(1)(1) should be(alignedPoints(1)(1) +- 0.0001)
        transformedPoints(2)(0) should be(alignedPoints(2)(0) +- 0.0001)
        transformedPoints(2)(1) should be(alignedPoints(2)(1) +- 0.0001)
      }
    }
  }

  describe("A 3D rigid landmark based registration") {

    val path = getClass.getResource("/facemesh.stl").getPath
    val mesh = MeshIO.readMesh(new File(path)).get

    val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
    val trans = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)

    val rigidTransformed = mesh transform trans

    val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.pointSet.points.zip(rigidTransformed.pointSet.points).toIndexedSeq)

    //should not test on parameters here since many euler angles can lead to the same rotation matrix
    val rigidRegTransformed = mesh transform regResult
    it("can retrieve correct parameters") {

      for ((p, i) <- rigidRegTransformed.pointSet.points.zipWithIndex) {
        val id = PointId(i)
        p(0) should be(rigidTransformed.pointSet.point(id)(0) +- 0.0001)
        p(1) should be(rigidTransformed.pointSet.point(id)(1) +- 0.0001)
        p(2) should be(rigidTransformed.pointSet.point(id)(2) +- 0.0001)
      }
    }

    it("Rigid Transformation forth and back of a mesh gives the same points") {
      val inverseTrans = regResult.asInstanceOf[RigidTransformation[_3D]].inverse
      val transformed = mesh.transform(regResult).transform(inverseTrans)

      for ((p, i) <- transformed.pointSet.points.zipWithIndex) {
        val id = PointId(i)
        p(0) should be(mesh.pointSet.point(id)(0) +- 0.0001)
        p(1) should be(mesh.pointSet.point(id)(1) +- 0.0001)
        p(2) should be(mesh.pointSet.point(id)(2) +- 0.0001)
      }
    }

    it("can retrieve correct transformation when requested with a different center") {

      // pick any center
      val anyCenter = Point(1254, 488, 78)
      val newCenterRegResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.pointSet.points.zip(rigidTransformed.pointSet.points).toIndexedSeq, anyCenter)

      val rigidRegNewCenterTransformed = mesh transform newCenterRegResult
      for ((p, i) <- rigidRegNewCenterTransformed.pointSet.points.zipWithIndex) {
        val id = PointId(i)
        p(0) should be(rigidTransformed.pointSet.point(id)(0) +- 0.0001)
        p(1) should be(rigidTransformed.pointSet.point(id)(1) +- 0.0001)
        p(2) should be(rigidTransformed.pointSet.point(id)(2) +- 0.0001)
      }
    }
  }

  describe("A 2D similarity landmark based registration") {
    it("can transform the points appropriately") {
      val points: IndexedSeq[Point[_2D]] = IndexedSeq(Point(0.0, 0.0), Point(1.0, 4.0), Point(2.0, 0.0))

      val c = Point(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotationParams = DenseVector[Double](-angle)
        val transParams = DenseVector[Double](1.0, 1.5)

        val scalingFactor = scala.util.Random.nextDouble()
        val productParams = DenseVector.vertcat(DenseVector.vertcat(transParams, rotationParams), DenseVector(scalingFactor))

        val productSpace = RigidTransformationSpace[_2D](c).product(ScalingSpace[_2D])

        val transformedPoints = points.map((pt: Point[_2D]) => productSpace.transformForParameters(productParams)(pt))

        val regResult = LandmarkRegistration.similarity2DLandmarkRegistration(points.zip(transformedPoints))

        val alignedPoints = points.map(regResult)
        transformedPoints(0)(0) should be(alignedPoints(0)(0) +- 0.0001)
        transformedPoints(0)(1) should be(alignedPoints(0)(1) +- 0.0001)
        transformedPoints(1)(0) should be(alignedPoints(1)(0) +- 0.0001)
        transformedPoints(1)(1) should be(alignedPoints(1)(1) +- 0.0001)
        transformedPoints(2)(0) should be(alignedPoints(2)(0) +- 0.0001)
        transformedPoints(2)(1) should be(alignedPoints(2)(1) +- 0.0001)
      }
    }
  }

  describe("A 3D similarity landmark based registration") {
    it("can transform the mesh appropriately") {

      val path = getClass.getResource("/facemesh.stl").getPath
      val mesh = MeshIO.readMesh(new File(path)).get

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI, 2.0)
      val trans = RigidTransformationSpace[_3D]().product(ScalingSpace[_3D]).transformForParameters(parameterVector)

      val translatedRotatedScaled = mesh transform trans

      val regResult = LandmarkRegistration.similarity3DLandmarkRegistration(mesh.pointSet.points.zip(translatedRotatedScaled.pointSet.points).toIndexedSeq)

      //should not test on parameters here since many euler angles can lead to the same rotation matrix
      val regSim = mesh transform regResult

      for ((p, i) <- regSim.pointSet.points.zipWithIndex.take(100)) {
        val id = PointId(i)
        p(0) should be(translatedRotatedScaled.pointSet.point(id)(0) +- 0.0001)
        p(1) should be(translatedRotatedScaled.pointSet.point(id)(1) +- 0.0001)
        p(2) should be(translatedRotatedScaled.pointSet.point(id)(2) +- 0.0001)
      }
    }
  }

  describe("A 2D image registration") {
    it("Recovers the correct parameters for a translation transform") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = discreteFixedImage.interpolate(3)

      val domain = discreteFixedImage.domain

      val regConf = RegistrationConfiguration[_2D, TranslationSpace[_2D]](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.0000001, false)),
        optimizer = LBFGSOptimizer(numIterations = 300),
        metric = MeanSquaresMetric(UniformSampler(domain.boundingBox, 4000)),
        transformationSpace = TranslationSpace[_2D],
        regularizer = L2Regularizer,
        regularizationWeight = 0.0)

      val translationParams = DenseVector[Double](-10.0, 5.0)
      val translationTransform = regConf.transformationSpace.transformForParameters(translationParams)
      val transformedLena = fixedImage compose translationTransform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      regResult.parameters(0) should be(translationParams(0) +- 0.01)
      regResult.parameters(1) should be(translationParams(1) +- 0.01)
    }

    it("Recovers the correct parameters for a rotation transform") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = discreteFixedImage.interpolate(3)

      val domain = discreteFixedImage.domain
      val center = ((domain.boundingBox.oppositeCorner - domain.origin) * 0.5).toPoint

      val regConf = RegistrationConfiguration[_2D, RotationSpace[_2D]](

        optimizer = GradientDescentOptimizer(numIterations = 300, stepLength = 1e-4),
        metric = MeanSquaresMetric(UniformSampler(domain.boundingBox, 4000)),
        transformationSpace = RotationSpace[_2D](center),
        regularizer = L2Regularizer,
        regularizationWeight = 0.0)

      val rotationParams = DenseVector[Double](math.Pi / 8.0)
      val transform = regConf.transformationSpace.transformForParameters(rotationParams)
      val transformedLena = fixedImage compose transform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      regResult.parameters(0) should be(rotationParams(0) +- 0.01)
    }

    it("Recovers the correct parameters for a gp transform") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = discreteFixedImage.interpolate(3)

      val domain = discreteFixedImage.domain

      val gp = GaussianProcess(Field(RealSpace[_2D], (_: Point[_2D]) => Vector.zeros[_2D]), DiagonalKernel(GaussianKernel[_2D](50.0) * 50.0, 2))
      val sampler = UniformSampler(domain.boundingBox, numberOfPoints = 200)
      val lowRankGp = LowRankGaussianProcess.approximateGP(gp, sampler, numBasisFunctions = 3)
      val regConf = RegistrationConfiguration[_2D, GaussianProcessTransformationSpace[_2D]](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.0000001, false)),
        optimizer = LBFGSOptimizer(numIterations = 300),
        metric = MeanSquaresMetric(UniformSampler(domain.boundingBox, 4000)),
        transformationSpace = GaussianProcessTransformationSpace(lowRankGp),
        regularizer = L2Regularizer,
        regularizationWeight = 0.0)

      val gpParams = DenseVector.ones[Double](lowRankGp.rank)
      val groundTruthTransform = regConf.transformationSpace.transformForParameters(gpParams)
      val transformedLena = fixedImage compose groundTruthTransform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      for (i <- 0 until regResult.parameters.size) {
        regResult.parameters(i) should be(gpParams(0) +- 0.1)
      }
    }

    it("Recovers the correct parameters for a gp transform with a nn interpolated gp") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(testImgUrl)).get
      val fixedImage = discreteFixedImage.interpolate(3)

      val domain = discreteFixedImage.domain

      val gp = GaussianProcess(Field(RealSpace[_2D], (_: Point[_2D]) => Vector.zeros[_2D]), DiagonalKernel(GaussianKernel[_2D](50.0) * 50.0, 2))
      val sampler = UniformSampler(domain.boundingBox, numberOfPoints = 200)
      val lowRankGp = LowRankGaussianProcess.approximateGP(gp, sampler, numBasisFunctions = 3)
      val nnInterpolatedGp = lowRankGp.discretize(domain).interpolateNearestNeighbor
      val regConf = RegistrationConfiguration[_2D, GaussianProcessTransformationSpace[_2D]](
        //optimizer = GradientDescentOptimizer(GradientDescentConfiguration(200, 0.0000001, false)),
        optimizer = LBFGSOptimizer(numIterations = 300),
        metric = MeanSquaresMetric(UniformSampler(domain.boundingBox, 4000)),
        transformationSpace = GaussianProcessTransformationSpace(nnInterpolatedGp),
        regularizer = L2Regularizer,
        regularizationWeight = 0.0)

      val gpParams = DenseVector.ones[Double](lowRankGp.rank)
      val groundTruthTransform = regConf.transformationSpace.transformForParameters(gpParams)
      val transformedLena = fixedImage compose groundTruthTransform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      for (i <- 0 until regResult.parameters.size) {
        regResult.parameters(i) should be(gpParams(0) +- 0.1)
      }
    }

  }

  describe("A 3D image registration") {
    val testImgUrl = getClass.getResource("/3ddm.nii").getPath
    val discreteFixedImage = ImageIO.read3DScalarImage[Float](new File(testImgUrl)).get
    val fixedImage = discreteFixedImage.interpolate(3)

    val domain = discreteFixedImage.domain

    it("Recovers the correct parameters for a translation transform") {

      val translationParams = DenseVector[Double](-10.0, 0.0, 0.0)
      val translationTransform = TranslationSpace[_3D].transformForParameters(translationParams)
      val transformed = fixedImage compose translationTransform

      val regConf = RegistrationConfiguration[_3D, TranslationSpace[_3D]](
        optimizer = LBFGSOptimizer(numIterations = 300),
        metric = MeanSquaresMetric(UniformSampler(domain.boundingBox, 20000)),
        transformationSpace = TranslationSpace[_3D],
        regularizer = L2Regularizer,
        regularizationWeight = 0.0)

      val regResult = Registration.registration(regConf)(transformed, fixedImage)
      regResult.parameters(0) should be(translationParams(0) +- 0.01)
      regResult.parameters(1) should be(translationParams(1) +- 0.01)
      regResult.parameters(2) should be(translationParams(2) +- 0.01)
    }

  }
}
