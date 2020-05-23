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

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{Field, Vectorizer}
import scalismo.geometry.EuclideanVector.VectorVectorizer
import scalismo.geometry.{_1D, _2D, _3D, EuclideanVector, Point}
import scalismo.transformations.TransformationSpace.ParameterVector
import scalismo.transformations.{ParametricTransformation, TransformationSpace}
import scalismo.statisticalmodel.LowRankGaussianProcess
import scalismo.statisticalmodel.LowRankGaussianProcess.Eigenpair
import scalismo.transformations.ParametricTransformation.JacobianField

case class GaussianProcessTransformation[D](gp: LowRankGaussianProcess[D, EuclideanVector[D]], alpha: ParameterVector)(
  implicit val vectorizer: Vectorizer[EuclideanVector[D]]
) extends ParametricTransformation[D] {

  val instance = gp.instance(alpha)
  val parameters = alpha

  override val domain = gp.domain

  override val f = (x: Point[D]) => {
    val newPointAsVector = instance(x)
    x + newPointAsVector
  }

  override def derivativeWRTParameters: JacobianField[D] = {

    /**
     * The derivativeWRTParameters matrix of a sample, with respect to the given parameters.
     * @return
     */
    val jacobian = (x: Point[D]) => {
      val dim = x.dimensionality
      val J = DenseMatrix.zeros[Double](dim, gp.klBasis.size)
      (0 until gp.rank).map(i => {
        val Eigenpair(lambda_i, phi_i) = gp.klBasis(i)
        J(::, i) := vectorizer.vectorize(phi_i(x)) * math.sqrt(lambda_i)
      })
      J
    }
    Field(domain, jacobian)
  }

  override def numberOfParameters: Int = gp.rank
}

object GaussianProcessTransformation {
  def apply[D](gp: LowRankGaussianProcess[D, EuclideanVector[D]],
               alpha: TransformationSpace.ParameterVector)(implicit vectorizer: Vectorizer[EuclideanVector[D]]) = {
    new GaussianProcessTransformation[D](gp, alpha)
  }
}

object GaussianProcessTransformation1D {
  def apply(gp: LowRankGaussianProcess[_1D, EuclideanVector[_1D]],
            alpha: TransformationSpace.ParameterVector): GaussianProcessTransformation[_1D] = {
    new GaussianProcessTransformation[_1D](gp, alpha)
  }
}

object GaussianProcessTransformation2D {
  def apply(gp: LowRankGaussianProcess[_2D, EuclideanVector[_2D]],
            alpha: TransformationSpace.ParameterVector): GaussianProcessTransformation[_2D] = {
    new GaussianProcessTransformation[_2D](gp, alpha)
  }
}

object GaussianProcessTransformation3D {
  def apply(gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]],
            alpha: TransformationSpace.ParameterVector): GaussianProcessTransformation[_3D] = {
    new GaussianProcessTransformation[_3D](gp, alpha)
  }
}

case class GaussianProcessTransformationSpace[D](gp: LowRankGaussianProcess[D, EuclideanVector[D]])(
  implicit
  vectorizer: VectorVectorizer[D]
) extends TransformationSpace[D] {

  override type T[D] = GaussianProcessTransformation[D]
  override val domain = gp.domain

  override def identityTransformation = {
    transformationForParameters(DenseVector.zeros[Double](numberOfParameters))
  }

  override def numberOfParameters = gp.rank

  override def transformationForParameters(p: ParameterVector) = GaussianProcessTransformation[D](gp, p)

}

object GaussianProcessTransformationSpace1D {
  def apply(gp: LowRankGaussianProcess[_1D, EuclideanVector[_1D]])(implicit vectorizer: VectorVectorizer[_1D]) = {
    GaussianProcessTransformationSpace[_1D](gp)
  }
}

object GaussianProcessTransformationSpace2D {
  def apply(gp: LowRankGaussianProcess[_2D, EuclideanVector[_2D]])(implicit vectorizer: VectorVectorizer[_2D]) = {
    GaussianProcessTransformationSpace[_2D](gp)
  }
}

object GaussianProcessTransformationSpace3D {
  def apply(gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]])(implicit vectorizer: VectorVectorizer[_3D]) = {
    GaussianProcessTransformationSpace[_3D](gp)
  }
}
