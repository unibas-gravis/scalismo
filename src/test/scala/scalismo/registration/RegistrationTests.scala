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

import breeze.linalg.DenseVector
import scalismo.common.interpolation.{BSplineImageInterpolator, BSplineImageInterpolator2D, BSplineImageInterpolator3D, NearestNeighborInterpolator}
import scalismo.common.{EuclideanSpace2D, Field, PointId, RealSpace}
import scalismo.geometry.*
import scalismo.image.{DiscreteImageDomain2D, DiscreteImageDomain3D}
import scalismo.io.{ImageIO, MeshIO}
import scalismo.kernels.{DiagonalKernel, GaussianKernel}
import scalismo.numerics.{GridSampler, LBFGSOptimizer, UniformSampler}
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess}
import scalismo.transformations.*
import scalismo.utils.Random
import scalismo.{ScalismoTestSuite, numerics}

import java.io.File
import java.net.URLDecoder
import scala.language.implicitConversions

class RegistrationTests extends ScalismoTestSuite {

  implicit val random: Random = Random(42)

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A 2D rigid landmark based registration") {
    it("can retrieve correct parameters") {
      val points: IndexedSeq[Point[_2D]] = IndexedSeq(Point(0.0, 0.0), Point(1.0, 4.0), Point(2.0, 0.0))

      val c = Point(1.0, 4 / 3.0)
      for (angle <- (1 until 16).map(i => math.Pi / i)) {
        val rotation = Rotation2D(-angle, Point2D(0, 0))
        val translation = Translation2D(EuclideanVector2D(1.0, 1.5))
        val compositeTransformation = TranslationAfterRotation2D(translation, rotation)

        val transformedPoints =
          points.map((pt: Point[_2D]) => compositeTransformation(pt))

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

    val translation = Translation3D(EuclideanVector3D(1.5, 1.0, 3.5))
    val rotation = Rotation3D(Math.PI, -Math.PI / 2.0, -Math.PI, center = Point3D(0, 0, 0))
    val trans = TranslationAfterRotation3D(translation, rotation)

    val rigidTransformed = mesh transform trans

    val regResult = LandmarkRegistration.rigid3DLandmarkRegistration(
      mesh.pointSet.points.zip(rigidTransformed.pointSet.points).toIndexedSeq,
      Point(0, 0, 0)
    )

    // should not test on parameters here since many euler angles can lead to the same rotation matrix
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
      val inverseTrans = regResult.inverse
      val transformed = mesh.transform(regResult).transform(inverseTrans)

      for ((p, i) <- transformed.pointSet.points.zipWithIndex) {
        val id = PointId(i)
        p(0) should be(mesh.pointSet.point(id)(0) +- 0.0001)
        p(1) should be(mesh.pointSet.point(id)(1) +- 0.0001)
        p(2) should be(mesh.pointSet.point(id)(2) +- 0.0001)
      }
    }

    it("can retrieve correct transformations when requested with a different center") {

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
        val rotation = Rotation2D(-angle, c)
        val translation = Translation2D(EuclideanVector2D(1.0, 1.5))

        val scalingFactor = 2.0 // scala.util.Random.nextDouble()

        val similarityTransformation =
          TranslationAfterScalingAfterRotation2D(translation, Scaling2D(scalingFactor), rotation)

        val transformedPoints =
          points.map((pt: Point[_2D]) => similarityTransformation(pt))

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

      val translation = Translation3D(EuclideanVector3D(1.5, 1.0, 3.5))
      val rotation = Rotation3D(Math.PI, -Math.PI / 2.0, -Math.PI, Point3D(0, 0, 0))
      val scaling = Scaling3D(2.0)
      val trans = TranslationAfterScalingAfterRotation3D(translation, scaling, rotation)

      val translatedRotatedScaled = mesh transform trans

      val regResult = LandmarkRegistration.similarity3DLandmarkRegistration(
        mesh.pointSet.points.zip(translatedRotatedScaled.pointSet.points).toIndexedSeq,
        Point(0, 0, 0)
      )

      // should not test on parameters here since many euler angles can lead to the same rotation matrix
      val regSim = mesh transform regResult

      for ((p, i) <- regSim.pointSet.points.zipWithIndex.take(100)) {
        val id = PointId(i)
        p(0) should be(translatedRotatedScaled.pointSet.point(id)(0) +- 0.0001)
        p(1) should be(translatedRotatedScaled.pointSet.point(id)(1) +- 0.0001)
        p(2) should be(translatedRotatedScaled.pointSet.point(id)(2) +- 0.0001)
      }
    }
  }
}
