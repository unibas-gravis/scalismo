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
import breeze.numerics._
import scalismo.common.{DifferentiableField, Field, Scalar}
import scalismo.geometry._
import scalismo.image.{DiscreteImageDomain, StructuredPoints}
import scalismo.numerics._
import scalismo.registration.RegistrationMetric.ValueAndDerivative
import scalismo.utils.{Memoize, Random}

/**
 * Implementation of the Mutual Information Metric, described in the following paper:
 *
 * Mattes, David, et al. "PET-CT image registration in the chest using free-form deformations."
 * IEEE transactions on medical imaging 22.1 (2003): 120-128.
 *
 * @param fixedImage The fixed image
 * @param fixedImageDomain The domain of the fixed image. All grid points of the domain are used to compute image characteristics
 *                         such as e.g. the minimum/maximum value, etc.
 * @param movingImage The moving image
 * @param transformationSpace The transformation space that is used
 * @param sampler The sampler, which samples the points on which the mutual information is computed. For this metric the
 *                recommended choice is a random sampler (which combined with a gradient descent algorithm leads to a stochastic gradient descent.
 * @param numberOfBins The number of bins used for the intensity histograms (which approximates the joint distribution)
 */
case class MutualInformationMetric[D: NDSpace, A: Scalar](fixedImage: Field[D, A],
                                                          fixedImageDomain: DiscreteImageDomain[D],
                                                          movingImage: DifferentiableField[D, A],
                                                          transformationSpace: TransformationSpace[D],
                                                          sampler: Sampler[D],
                                                          numberOfBins: Int = 30)(implicit rng: Random)
    extends ImageMetric[D, A] {

  type JointHistogram = (Int, Int) => Double
  type JointHistogramDerivative = (Int, Int) => DenseVector[Double]
  type MarginalHistogram = Int => Double

  val scalar = Scalar[A]

  override val ndSpace: NDSpace[D] = implicitly[NDSpace[D]]

  private val zeroOrderSpline = BSpline.nthOrderBSpline(0) _
  private val secondOrderSpline = BSpline.nthOrderBSpline(2) _
  private val thirdOrderSpline = BSpline.nthOrderBSpline(3) _

  // we choose a reasonably sized number of points from the fixed image domain to compute the image characteristics.
  // All the computations are done only once, when the metric is computed. Hence it is okay to use a rather large number of points
  private val fixedImagePoints =
    UniformSampler(fixedImageDomain.boundingBox, numberOfPoints = 100000).sample().map(_._1)

  private def minMaxValue(img: Field[D, A]): (Double, Double) = {

    val values = for (pt <- fixedImagePoints if img.isDefinedAt(pt)) yield scalar.toDouble(img(pt))
    (values.min, values.max)
  }

  private val (minValueFixedImage, maxValueFixedImage) = minMaxValue(fixedImage)
  private val (minValueMovingImage, maxValueMovingImage) = minMaxValue(movingImage)

  private val binDeltaFixedImage = maxValueFixedImage + 1 - minValueFixedImage
  private val binSizeFixedImage = binDeltaFixedImage / numberOfBins
  private val binDeltaMovingImage = maxValueMovingImage + 1 - minValueMovingImage
  private val binSizeMovingImage = binDeltaMovingImage / numberOfBins

  private val numberOfParameters: Int = transformationSpace.parametersDimensionality

  def _computeJointHistogram(parameters: DenseVector[Double], points: Seq[Point[D]]): JointHistogram = {

    val transform = Memoize(transformationSpace.transformForParameters(parameters), cacheSizeHint = points.size + 100)

    val jointHistogramValues =
      for (l <- (0 until numberOfBins).par;
           k <- (0 until numberOfBins)) yield {

        val probValues = for (point <- points) yield {

          val transformedPoint = transform(point)

          val termRef = zeroOrderSpline(
            k - (scalar.toDouble(fixedImage(point)) - minValueFixedImage) / binSizeFixedImage
          )

          if (Math.abs(termRef) > 1e-10 && movingImage.isDefinedAt(transformedPoint)) {

            val termTest = thirdOrderSpline(
              l - (scalar.toDouble(movingImage(transformedPoint)) - minValueMovingImage) / binSizeMovingImage
            )

            termRef * termTest
          } else {
            0.0
          }
        }
        val probValue = probValues.sum
        ((l, k), probValue)
      }

    val histogramValueMap = jointHistogramValues.toIndexedSeq.toMap

    val normalizationTerm = jointHistogramValues.foldLeft(0.0)((acc, e) => acc + e._2)

    (l: Int, k: Int) => histogramValueMap((l, k)) / normalizationTerm
  }

  private lazy val computeJointHistogram = Memoize.memfun2(_computeJointHistogram _, cacheSizeHint = 100)

  private def _computeMarginalHistogramMovingImage(params: DenseVector[Double],
                                                   points: Seq[Point[D]]): Int => Double = {

    val jointHistogram = computeJointHistogram(params, points)

    val probValues = for (l <- 0 until numberOfBins) yield {

      val probValue = (0 until numberOfBins).foldLeft(0.0)((acc, k) => acc + jointHistogram(l, k))

      (l, probValue)
    }

    val plMap = probValues.toMap

    (l: Int) => plMap(l)
  }

  private lazy val computeMarginalHistogramMovingImage =
    Memoize.memfun2(_computeMarginalHistogramMovingImage _, cacheSizeHint = 100)

  private val marginalHistogramFixedImage: MarginalHistogram = {

    val probValues = for (k <- 0 until numberOfBins) yield {

      val histogramValue = fixedImagePoints.foldLeft(0.0)((acc, point) => {
        val termRef = k - (scalar.toDouble(fixedImage(point)) - minValueFixedImage) / binSizeFixedImage
        acc + zeroOrderSpline(termRef)
      })

      (k, histogramValue)
    }

    val alpha = probValues.map(_._2).sum
    val probValuesMap = probValues.toMap

    (k: Int) => probValuesMap(k) / alpha
  }

  private def _computeJointHistogramDerivative(params: DenseVector[Double],
                                               points: Seq[Point[D]]): JointHistogramDerivative = {

    val transform = Memoize(transformationSpace.transformForParameters(params), points.size + 100)
    val zeroVec = DenseVector.zeros[Double](numberOfParameters)

    val derivs = for (l <- (0 until numberOfBins).par; k <- (0 until numberOfBins)) yield {

      val derivsForPoints = for (point <- points) yield {

        val transformedPoint = transform(point)

        if (movingImage.isDefinedAt(transformedPoint)) {
          val termRefSpline: Double = zeroOrderSpline(
            k - (scalar.toDouble(fixedImage(point)) - minValueFixedImage) / binSizeFixedImage
          )

          val value = if (Math.abs(termRefSpline) > 1e-10) {

            val termTest = l - (scalar
              .toDouble(movingImage(transformedPoint)) - minValueMovingImage) / binSizeMovingImage

            val termTestSpline: Double = secondOrderSpline(termTest + 0.5) - secondOrderSpline(termTest - 0.5)

            if (Math.abs(termTestSpline) > 1e-10) {
              val termTestDerivative: DenseVector[Double] = -movingImage.differentiate(transformedPoint).toBreezeVector
              val termTransSpace: transformationSpace.JacobianImage =
                transformationSpace.takeDerivativeWRTParameters(params)
              termTransSpace(point).t * termTestDerivative * termRefSpline * termTestSpline
            } else {
              zeroVec
            }
          } else {
            zeroVec
          }
          Some(value)
        } else {
          None
        }
      }
      val derivsAtValidPoints = derivsForPoints.collect { case Some(v) => v }
      val deriv = derivsAtValidPoints.foldLeft(zeroVec)((acc, v) => acc + v)

      val scaledDeriv = (1.0 / (derivsAtValidPoints.size * binSizeMovingImage)) * deriv
      ((l, k), scaledDeriv)
    }

    val map = derivs.toMap
    (l, k) => map((l, k))
  }

  private lazy val computeJointHistogramDerivative = Memoize.memfun2(_computeJointHistogramDerivative _, 100)

  private def _computeMarginalHistogramDerivativeMovingImage(params: DenseVector[Double],
                                                             points: Seq[Point[D]]): Int => DenseVector[Double] = {

    /** creating and filling derivative vector (gradient) */
    val derivJointHistogramForParams = computeJointHistogramDerivative(params, points)

    val derivs = for (l <- 0 until numberOfBins) yield {

      val deriv = (0 until numberOfBins).foldLeft(DenseVector.zeros[Double](numberOfParameters))((acc, k) =>
        acc + derivJointHistogramForParams(l, k)
      )

      (l, deriv)
    }

    val derivMap = derivs.toMap
    (l: Int) => derivMap(l)
  }

  private lazy val computeMarginalHistogramDerivativeMovingImage =
    Memoize.memfun2(_computeMarginalHistogramDerivativeMovingImage _, cacheSizeHint = 100)

  /**
   * Computes the value of the mutual information for the given parameters
   */
  override def value(params: DenseVector[Double]): Double = {

    val samplePoints = sampler.sample().map(_._1)

    val jointHistogram = computeJointHistogram(params, samplePoints)
    val marginalHistogramMoving = computeMarginalHistogramMovingImage(params, samplePoints)

    val mivalues = for (k <- (0 until numberOfBins).par; l <- 0 until numberOfBins) yield {

      if (marginalHistogramFixedImage(k) != 0 && marginalHistogramMoving(l) != 0 && jointHistogram(l, k) != 0) {

        jointHistogram(l, k) * log(jointHistogram(l, k) / (marginalHistogramMoving(l) * marginalHistogramFixedImage(k)))
      } else {
        0.0
      }

    }

    val mutualInfo = mivalues.sum
    -mutualInfo

  }

  /**
   * Computes the derivate of the Mutual Information for the given parameters
   */
  def derivative(params: DenseVector[Double]): DenseVector[Double] = {

    val samplePoints = sampler.sample().map(_._1)

    /** creating and filling derivative vector (gradient) */
    val marginalHistMoving = computeMarginalHistogramMovingImage(params, samplePoints)
    val jointHist = computeJointHistogram(params, samplePoints)
    val jointHistDeriv = computeJointHistogramDerivative(params, samplePoints)
    val marginalHistDerivMoving = computeMarginalHistogramDerivativeMovingImage(params, samplePoints)

    val gradientValues = for (k <- (0 until numberOfBins).par; l <- 0 until numberOfBins) yield {

      if (marginalHistogramFixedImage(k) != 0 && marginalHistMoving(l) != 0) {
        (jointHistDeriv(l, k) * log(jointHist(l, k) / (marginalHistMoving(l) * marginalHistogramFixedImage(k)) + 1)
          - (jointHist(l, k) / marginalHistMoving(l)) * marginalHistDerivMoving(l))
      } else {
        DenseVector.zeros[Double](numberOfParameters)
      }

    }
    val gradient = gradientValues.foldLeft(DenseVector.zeros[Double](numberOfParameters))((acc, g) => acc + g)

    -gradient

  }

  /**
   * Computes the value and derivative for the given parameters in one go.
   */
  def valueAndDerivative(params: DenseVector[Double]): ValueAndDerivative = {

    val samplePoints = sampler.sample().map(_._1)

    val marginalHistMoving = computeMarginalHistogramMovingImage(params, samplePoints)
    val jointHist = computeJointHistogram(params, samplePoints)
    val joingHistDeriv = computeJointHistogramDerivative(params, samplePoints)
    val marginalHistDerivMoving = computeMarginalHistogramDerivativeMovingImage(params, samplePoints)
    val marginalHistogramRef = marginalHistogramFixedImage

    val mivalues = for (k <- (0 until numberOfBins).par; l <- 0 until numberOfBins) yield {

      if (marginalHistogramRef(k) != 0 && marginalHistMoving(l) != 0 && jointHist(l, k) != 0) {

        jointHist(l, k) * log(jointHist(l, k) / (marginalHistMoving(l) * marginalHistogramRef(k)))
      } else {
        0.0
      }

    }

    val mutualInfo = mivalues.sum

    /** creating and filling derivative vector (gradient) */
    val gradientValues = for (k <- (0 until numberOfBins); l <- 0 until numberOfBins) yield {

      if (marginalHistogramRef(k) != 0 && marginalHistMoving(l) != 0) {
        (joingHistDeriv(l, k) * log(jointHist(l, k) / (marginalHistMoving(l) * marginalHistogramRef(k)) + 1)
          - (jointHist(l, k) / marginalHistMoving(l)) * marginalHistDerivMoving(l))
      } else {
        DenseVector.zeros[Double](numberOfParameters)
      }

    }
    val gradient = gradientValues.foldLeft(DenseVector.zeros[Double](numberOfParameters))((acc, g) => acc + g)

    ValueAndDerivative(-mutualInfo, -gradient)

  }
}
