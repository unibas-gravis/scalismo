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
import java.net.URLDecoder

import breeze.linalg.DenseVector
import scalismo.common.interpolation.{BSplineImageInterpolator, BSplineImageInterpolator2D, BSplineImageInterpolator3D}
import scalismo.{numerics, ScalismoTestSuite}
import scalismo.common.{Field, NearestNeighborInterpolator, PointId, RealSpace}
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain, DiscreteImageDomain2D, DiscreteImageDomain3D, StructuredPoints}
import scalismo.io.{ImageIO, MeshIO}
import scalismo.kernels.{DiagonalKernel, GaussianKernel}
import scalismo.numerics.{GridSampler, LBFGSOptimizer, UniformSampler}
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess}
import scalismo.utils.Random

import scala.language.implicitConversions

class RegistrationTests extends ScalismoTestSuite {

  implicit val random = Random(42)

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

        val regResult =
          LandmarkRegistration.rigid2DLandmarkRegistration(points.zip(transformedPoints), center = Point2D(0, 0))

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
    val mesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get

    val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI)
    val trans = RigidTransformationSpace[_3D]().transformForParameters(parameterVector)

    val rigidTransformed = mesh transform trans

    val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(
      mesh.pointSet.points.zip(rigidTransformed.pointSet.points).toIndexedSeq,
      Point(0, 0, 0)
    )

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
      val newCenterRegResult = LandmarkRegistration.rigid3DLandmarkRegistration(
        mesh.pointSet.points.zip(rigidTransformed.pointSet.points).toIndexedSeq,
        anyCenter
      )

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
        val productParams =
          DenseVector.vertcat(DenseVector.vertcat(transParams, rotationParams), DenseVector(scalingFactor))

        val productSpace = RigidTransformationSpace[_2D](c).product(ScalingSpace[_2D])

        val transformedPoints = points.map((pt: Point[_2D]) => productSpace.transformForParameters(productParams)(pt))

        val regResult =
          LandmarkRegistration.similarity2DLandmarkRegistration(points.zip(transformedPoints), center = Point2D(0, 0))

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
      val mesh = MeshIO.readMesh(new File(URLDecoder.decode(path, "UTF-8"))).get

      val parameterVector = DenseVector[Double](1.5, 1.0, 3.5, Math.PI, -Math.PI / 2.0, -Math.PI, 2.0)
      val trans = RigidTransformationSpace[_3D]().product(ScalingSpace[_3D]).transformForParameters(parameterVector)

      val translatedRotatedScaled = mesh transform trans

      val regResult = LandmarkRegistration.similarity3DLandmarkRegistration(
        mesh.pointSet.points.zip(translatedRotatedScaled.pointSet.points).toIndexedSeq,
        Point(0, 0, 0)
      )

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

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgUrl, "UTF-8"))).get
      val fixedImage = discreteFixedImage.interpolateDifferentiable(BSplineImageInterpolator2D[Float](2))
      val transformationSpace = TranslationSpace[_2D]
      val translationParams = DenseVector[Double](-10.0, 5.0)
      val translationTransform = transformationSpace.transformForParameters(translationParams)
      val transformedLena = fixedImage compose translationTransform

      val domain = discreteFixedImage.domain

      val metricSampler = GridSampler(DiscreteImageDomain2D(domain.boundingBox, IntVector(20, 20)))
      val regIt = Registration(
        MeanSquaresMetric(fixedImage, transformedLena, transformationSpace, metricSampler),
        L2Regularizer[_2D](transformationSpace),
        regularizationWeight = 0.0,
        LBFGSOptimizer(maxNumberOfIterations = 300)
      ).iterator(DenseVector.zeros[Double](transformationSpace.parametersDimensionality))

      val regResult = regIt.toSeq.last
      -regResult.parameters(0) should be(translationParams(0) +- 0.01)
      -regResult.parameters(1) should be(translationParams(1) +- 0.01)
    }

    it("Recovers the correct parameters for a rotation transform") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath
      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgUrl, "UTF-8"))).get
      val fixedImage = discreteFixedImage.interpolateDifferentiable(BSplineImageInterpolator2D[Float](3))
      val domain = discreteFixedImage.domain
      val center = ((domain.boundingBox.oppositeCorner - domain.origin) * 0.5).toPoint
      val transformationSpace = RotationSpace[_2D](center)
      val rotationParams = DenseVector[Double](math.Pi / 8.0)
      val transform = transformationSpace.transformForParameters(rotationParams)
      val transformedLena = fixedImage compose transform

      val metricSampler = GridSampler(DiscreteImageDomain2D(domain.boundingBox, IntVector(20, 20)))
      val metric = MeanSquaresMetric(transformedLena, fixedImage, transformationSpace, metricSampler)

      val regIter = Registration(
        metric,
        L2Regularizer(transformationSpace),
        0.0,
        numerics.LBFGSOptimizer(maxNumberOfIterations = 100)
      ).iterator(DenseVector.zeros[Double](transformationSpace.parametersDimensionality))

      val regResult = regIter.toSeq.last
      regResult.parameters(0) should be(rotationParams(0) +- 0.01)
    }

    it("Recovers the correct parameters for a gp transform") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgUrl, "UTF-8"))).get
      val fixedImage = discreteFixedImage.interpolateDifferentiable(BSplineImageInterpolator2D[Float](3))

      val domain = discreteFixedImage.domain
      val gp = GaussianProcess(Field(RealSpace[_2D], (_: Point[_2D]) => EuclideanVector.zeros[_2D]),
                               DiagonalKernel(GaussianKernel[_2D](50.0) * 50.0, 2))
      val sampler = UniformSampler(domain.boundingBox, numberOfPoints = 200)
      val lowRankGp = LowRankGaussianProcess.approximateGPNystrom(gp, sampler, numBasisFunctions = 3)
      val gpParams = DenseVector.ones[Double](lowRankGp.rank)
      val transformationSpace = GaussianProcessTransformationSpace(lowRankGp)

      val groundTruthTransform = transformationSpace.transformForParameters(gpParams)

      val transformedLena = fixedImage compose groundTruthTransform

      val metricSampler = GridSampler(DiscreteImageDomain2D(domain.boundingBox, IntVector(20, 20)))
      val metric = MeanSquaresMetric(transformedLena, fixedImage, transformationSpace, metricSampler)

      val regIt =
        Registration(metric, L2Regularizer(transformationSpace), 0.0, LBFGSOptimizer(maxNumberOfIterations = 300))
          .iterator(DenseVector.zeros[Double](transformationSpace.parametersDimensionality))

      val regResult = regIt.toSeq.last

      for (i <- 0 until regResult.parameters.size) {
        regResult.parameters(i) should be(gpParams(0) +- 0.1)
      }
    }

    it("Recovers the correct parameters for a gp transform with a nn interpolated gp") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgUrl, "UTF-8"))).get
      val fixedImage = discreteFixedImage.interpolateDifferentiable(BSplineImageInterpolator2D[Float](3))

      val domain = discreteFixedImage.domain

      val gp = GaussianProcess(Field(RealSpace[_2D], (_: Point[_2D]) => EuclideanVector.zeros[_2D]),
                               DiagonalKernel(GaussianKernel[_2D](50.0) * 50.0, 2))
      val sampler = UniformSampler(domain.boundingBox, numberOfPoints = 200)
      val lowRankGp = LowRankGaussianProcess.approximateGPNystrom(gp, sampler, numBasisFunctions = 3)
      val nnInterpolatedGp = lowRankGp.discretize(domain).interpolate(NearestNeighborInterpolator())

      val transformationSpace = GaussianProcessTransformationSpace(nnInterpolatedGp)
      val gpParams = DenseVector.ones[Double](lowRankGp.rank)
      val groundTruthTransform = transformationSpace.transformForParameters(gpParams)
      val transformedLena = fixedImage compose groundTruthTransform

      val metricSampler = GridSampler(DiscreteImageDomain2D(domain.boundingBox, IntVector(20, 20)))
      val metric = MeanSquaresMetric(transformedLena, fixedImage, transformationSpace, metricSampler)

      val regIt = Registration(
        metric,
        L2Regularizer(transformationSpace),
        regularizationWeight = 0.0,
        LBFGSOptimizer(maxNumberOfIterations = 300)
      ).iterator(DenseVector.zeros[Double](transformationSpace.parametersDimensionality))

      val regResult = regIt.toSeq.last
      for (i <- 0 until regResult.parameters.size) {
        regResult.parameters(i) should be(gpParams(0) +- 0.1)
      }
    }

    it("Recovers the correct parameters for a composed rigid and gp transform") {
      val testImgUrl = getClass.getResource("/dm128.vtk").getPath

      val discreteFixedImage = ImageIO.read2DScalarImage[Float](new File(URLDecoder.decode(testImgUrl, "UTF-8"))).get
      val fixedImage = discreteFixedImage.interpolateDifferentiable(BSplineImageInterpolator(3))

      val domain = discreteFixedImage.domain

      val gp = GaussianProcess(Field(RealSpace[_2D], (_: Point[_2D]) => EuclideanVector.zeros[_2D]),
                               DiagonalKernel(GaussianKernel[_2D](20.0) * 50.0, 2))
      val lowRankGp =
        LowRankGaussianProcess.approximateGPCholesky(domain, gp, 0.1, NearestNeighborInterpolator()).truncate(5)
      val translationSpace = TranslationSpace[_2D]
      val gpTransformationSpace = GaussianProcessTransformationSpace(lowRankGp)
      val transformationSpace = ProductTransformationSpace(translationSpace, gpTransformationSpace)
      val gtParams = DenseVector.vertcat(DenseVector.ones[Double](translationSpace.parametersDimensionality) * 10.0,
                                         DenseVector.ones[Double](gpTransformationSpace.parametersDimensionality) * 1.0)
      val groundTruthTransform = transformationSpace.transformForParameters(gtParams)
      val transformedLena = fixedImage compose groundTruthTransform
      val metricSampler = GridSampler(DiscreteImageDomain2D(domain.boundingBox, IntVector(20, 20)))
      val metric = MeanSquaresMetric(transformedLena, fixedImage, transformationSpace, metricSampler)

      val regIt = Registration(
        metric,
        L2Regularizer(transformationSpace),
        regularizationWeight = 0.0,
        LBFGSOptimizer(maxNumberOfIterations = 300)
      ).iterator(DenseVector.zeros[Double](transformationSpace.parametersDimensionality))
      val regItPrinting = for (it <- regIt) yield {
        println(it.value)
        println(it.parameters)
        it
      }

      val regResult = regItPrinting.toSeq.last
      for (i <- 0 until regResult.parameters.size) {
        regResult.parameters(i) should be(gtParams(i) +- 0.1)
      }
    }

  }

  describe("A 3D image registration") {
    val testImgUrl = getClass.getResource("/3ddm.nii").getPath
    val discreteFixedImage = ImageIO.read3DScalarImage[Float](new File(URLDecoder.decode(testImgUrl, "UTF-8"))).get
    val fixedImage = discreteFixedImage.interpolateDifferentiable(BSplineImageInterpolator3D[Float](3))

    val transformationSpace = TranslationSpace[_3D]
    val domain = discreteFixedImage.domain

    it("Recovers the correct parameters for a translation transform") {

      val translationParams = DenseVector[Double](-10.0, 0.0, 0.0)
      val translationTransform = TranslationSpace[_3D].transformForParameters(translationParams)
      val transformed = fixedImage compose translationTransform

      val metricSampler = GridSampler(DiscreteImageDomain3D(domain.boundingBox, IntVector(20, 20, 20)))
      val metric = MeanSquaresMetric(fixedImage, transformed, transformationSpace, metricSampler)

      val regIt = Registration(
        metric,
        L2Regularizer(transformationSpace),
        regularizationWeight = 0.0,
        LBFGSOptimizer(maxNumberOfIterations = 300)
      ).iterator(DenseVector.zeros[Double](transformationSpace.parametersDimensionality))

      val regResult = regIt.toSeq.last

      -regResult.parameters(0) should be(translationParams(0) +- 0.01)
      -regResult.parameters(1) should be(translationParams(1) +- 0.01)
      -regResult.parameters(2) should be(translationParams(2) +- 0.01)
    }

  }
}
