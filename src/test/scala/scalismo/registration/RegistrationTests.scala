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
import scalismo.geometry._
import scalismo.io.{ ImageIO, MeshIO }
import scalismo.numerics.{ GradientDescentOptimizer, LBFGSOptimizer, UniformSampler }

import scala.language.implicitConversions

class RegistrationTests extends ScalismoTestSuite {

  implicit def doubleToFloat(d: Double) = d.toFloat

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

    val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
    val trans = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)

    val rigidTransformed = mesh transform trans

    val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(mesh.points.zip(rigidTransformed.points).toIndexedSeq)

    //should not test on parameters here since many euler angles can lead to the same rotation matrix
    val rigidRegTransformed = mesh transform regResult
    it("can retrieve correct parameters") {

      for ((p, i) <- rigidRegTransformed.points.zipWithIndex) {
        p(0) should be(rigidTransformed.point(i)(0) +- 0.0001)
        p(1) should be(rigidTransformed.point(i)(1) +- 0.0001)
        p(2) should be(rigidTransformed.point(i)(2) +- 0.0001)
      }
    }

    it("Rigid Transformation forth and back of a mesh gives the same points ") {
      val inverseTrans = regResult.asInstanceOf[RigidTransformation[_3D]].inverse
      val tranformed = mesh.transform(regResult).transform(inverseTrans)

      for ((p, i) <- tranformed.points.zipWithIndex) {
        p(0) should be(mesh.point(i)(0) +- 0.0001)
        p(1) should be(mesh.point(i)(1) +- 0.0001)
        p(2) should be(mesh.point(i)(2) +- 0.0001)
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

      val parameterVector = DenseVector[Float](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI, 2f)
      val trans = RigidTransformationSpace[_3D]().product(ScalingSpace[_3D]).transformForParameters(parameterVector)

      val translatedRotatedScaled = mesh transform trans

      val regResult = LandmarkRegistration.similarity3DLandmarkRegistration(mesh.points.zip(translatedRotatedScaled.points).toIndexedSeq)

      //should not test on parameters here since many euler angles can lead to the same rotation matrix
      val regSim = mesh transform regResult

      for ((p, i) <- regSim.points.zipWithIndex.take(100)) {
        p(0) should be(translatedRotatedScaled.point(i)(0) +- 0.0001)
        p(1) should be(translatedRotatedScaled.point(i)(1) +- 0.0001)
        p(2) should be(translatedRotatedScaled.point(i)(2) +- 0.0001)
      }
    }
  }

  describe("A 2D image registration") {
    it("Recovers the correct parameters for a translation transfrom") {
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

      val translationParams = DenseVector[Float](-10.0, 5.0)
      val translationTransform = regConf.transformationSpace.transformForParameters(translationParams)
      val transformedLena = fixedImage compose translationTransform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      regResult.parameters(0) should be(translationParams(0) +- 0.01)
      regResult.parameters(1) should be(translationParams(1) +- 0.01)
    }

    it("Recovers the correct parameters for a rotation transfrom") {
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

      val rotationParams = DenseVector[Float](math.Pi / 8.0)
      val transform = regConf.transformationSpace.transformForParameters(rotationParams)
      val transformedLena = fixedImage compose transform
      val regResult = Registration.registration(regConf)(transformedLena, fixedImage)

      regResult.parameters(0) should be(rotationParams(0) +- 0.01)
    }
  }

  describe("A 3D image registration") {
    val testImgUrl = getClass.getResource("/3ddm.nii").getPath
    val discreteFixedImage = ImageIO.read3DScalarImage[Float](new File(testImgUrl)).get
    val fixedImage = discreteFixedImage.interpolate(3)

    val domain = discreteFixedImage.domain
    val origin = domain.origin
    val corener = domain.boundingBox.oppositeCorner
    val center = ((corener - origin) * 0.5).toPoint

    it("Recovers the correct parameters for a translation transfrom") {

      val translationParams = DenseVector[Float](-10.0, 0, 0)
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
