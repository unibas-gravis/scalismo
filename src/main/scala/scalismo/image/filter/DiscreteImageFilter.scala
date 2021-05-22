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
package scalismo.image.filter

import scalismo.common.interpolation.{BSplineImageInterpolator, BSplineImageInterpolator3D}
import scalismo.common.{Scalar, ScalarArray}
import scalismo.geometry._
import scalismo.image.DiscreteImage
import scalismo.utils.{CanConvertToVtk, ImageConversion}
import vtk.{vtkImageCast, vtkImageEuclideanDistance, vtkImageGaussianSmooth, vtkObjectBase}

import scala.reflect.ClassTag

object DiscreteImageFilter {

  /**
   * Computes a (signed) distance transform of the image.
   * @note The value that is returned is not the euclidean distance unless the image has unit spacing. Even worse, the distance might depend on the spacing of the image.
   */
  def distanceTransform[D: NDSpace: CanConvertToVtk: BSplineImageInterpolator.Create, A: Scalar: ClassTag](
    img: DiscreteImage[D, A]
  ): DiscreteImage[D, Float] = {

    val scalar = implicitly[Scalar[A]]

    def doDistanceTransformVTK(img: DiscreteImage[D, A]) = {
      implicit val ttFloat = TypeTag.Float
      val imgvtk = ImageConversion.imageToVtkStructuredPoints(img)

      val vtkdistTransform = new vtkImageEuclideanDistance()

      vtkdistTransform.SetMaximumDistance(100000)
      vtkdistTransform.SetAlgorithmToSaito()

      vtkdistTransform.InitializeOn()
      vtkdistTransform.ReleaseDataFlagOn()
      vtkdistTransform.SetConsiderAnisotropy(1)
      vtkdistTransform.SetInputData(imgvtk)

      // vtk gives double precision  we rather want to have float
      val caster = new vtkImageCast()
      caster.SetOutputScalarTypeToFloat()
      caster.SetInputConnection(vtkdistTransform.GetOutputPort())
      caster.Update()

      val dtvtk = caster.GetOutput()
      val dt = ImageConversion
        .vtkStructuredPointsToScalarImage[D, Float](dtvtk)
        .map { dt =>
          dt.map(v => math.sqrt(v).toFloat)
        }
        .get // this is safe here, as it can never fail since we converted back and forth

      caster.Delete()
      imgvtk.Delete()
      dtvtk.Delete()
      vtkdistTransform.Delete()
      System.gc() // make sure it deletes the intermediate resuls

      dt.interpolateDifferentiable(BSplineImageInterpolator(degree = 0)).discretize(img.domain, 0)
    }

    val dt1 = doDistanceTransformVTK(img)

    val invImg = img.map[A](v => if (v == 0) scalar.fromShort(1) else scalar.fromShort(0))
    val dt2 = doDistanceTransformVTK(invImg)

    val newPixelValues = dt1.values.zip(dt2.values).map { case (p1, p2) => p1 - p2 }.toArray

    DiscreteImage(dt1.domain, ScalarArray(newPixelValues))

  }

  /**
   * Smoothing of an image using a Gaussian filter kernel with the given stddev
   */
  def gaussianSmoothing[D: NDSpace, A: Scalar: ClassTag](img: DiscreteImage[D, A], stddev: Double)(
    implicit
    vtkConversion: CanConvertToVtk[D]
  ): DiscreteImage[D, A] = {

    val vtkImg = vtkConversion.toVtk[A](img)
    val dim = NDSpace[D].dimensionality
    val gaussianFilter = new vtkImageGaussianSmooth()
    gaussianFilter.SetInputData(vtkImg)
    val unitsAdjustedSpacing = img.domain.pointSet.spacing.map(s => stddev * (1f / s))

    unitsAdjustedSpacing.dimensionality match {
      case 2 => gaussianFilter.SetStandardDeviation(unitsAdjustedSpacing(0), unitsAdjustedSpacing(1))
      case 3 =>
        gaussianFilter.SetStandardDeviation(unitsAdjustedSpacing(0), unitsAdjustedSpacing(1), unitsAdjustedSpacing(2))
      case _ =>
        throw new IllegalArgumentException(
          s"Bad dimensionality for gaussianSmoothing. Got  $dim encountered but require 2 or 3."
        )
    }
    gaussianFilter.Update()
    val vtkRes = gaussianFilter.GetOutput()
    // it is save to call get here, as the error can only encounter when the pixel type is not supported.
    // But as we converted it ourselves to vtk, conversion is always possible.
    val imgRes = vtkConversion.fromVtk(vtkRes).get
    // prevent memory leaks caused by VTK
    vtkObjectBase.JAVA_OBJECT_MANAGER.gc(false)
    imgRes
  }
}
