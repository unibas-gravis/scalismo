package scalismo.statisticalmodel

import breeze.linalg.DenseMatrix
import scalismo.common.DiscreteDomain
import scalismo.geometry.*

/**
 * ideally used to represent linear effects that should be normalized between the training data. The realignment process
 * then builds a linear projection matrix that is applied to an existing model. the space of the effects that should be
 * normalized needs to be spanned by the returned matrix
 */
trait RealignExtendedBasis[D, Value]:

  /**
   * whether or not the default translation basis should also be used. that means false does not perform a translation
   * realignment. This in combination with getBasis allows for complete control of the projection matrix.
   */
  def useTranslation: Boolean

  /**
   * basis to span the kernel of the projection. for example, a translation alignment could be performed by spanning
   * that space with constant vectors for each cardinal direction.
   */
  def getBasis[DDomain[DD] <: DiscreteDomain[DD]](model: DiscreteLowRankGaussianProcess[D, DDomain, Value],
                                                  center: Point[D]
  ): DenseMatrix[Double]

/**
 * includes the additional default rotation centerpoint implementation which is useful to calculate the rotation basis.
 */
trait RealignExtendedBasisRotation[D, Value] extends RealignExtendedBasis[D, Value]:
  def centeredP[D: NDSpace, DDomain[DD] <: DiscreteDomain[DD]](domain: DDomain[D],
                                                               center: Point[D]
  ): DenseMatrix[Double] = {
    // build centered data matrix
    val x = DenseMatrix.zeros[Double](center.dimensionality, domain.pointSet.numberOfPoints)
    val c = center.toBreezeVector
    for (p, i) <- domain.pointSet.points.zipWithIndex do x(::, i) := p.toBreezeVector - c
    x
  }

object RealignExtendedBasis:
  /**
   * returns a projection basis for rotation - the tangential speed for the rotations around the three cardinal
   * directions.
   */
  given realignBasis3D: RealignExtendedBasisRotation[_3D, EuclideanVector[_3D]] with
    def useTranslation: Boolean = true
    def getBasis[DDomain[DD] <: DiscreteDomain[DD]](
      model: DiscreteLowRankGaussianProcess[_3D, DDomain, EuclideanVector[_3D]],
      center: Point[_3D]
    ): DenseMatrix[Double] = {
      val np = model.domain.pointSet.numberOfPoints
      val x = centeredP(model.domain, center)

      val pr = DenseMatrix.zeros[Double](np * 3, 3)
      // the derivative of the rotation matrices
      val dr = new DenseMatrix[Double](9,
                                       3,
                                       Array(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0,
                                             0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
      )
      // get tangential speed
      val dx = dr * x
      for i <- 0 until 3 do
        val v = dx(3 * i until 3 * i + 3, ::).toDenseVector
        pr(::, i) := v / breeze.linalg.norm(v)
      pr
    }

  /**
   * returns a projection basis for rotation - the tangential speed for the single 2d rotation.
   */
  given realignBasis2D: RealignExtendedBasisRotation[_2D, EuclideanVector[_2D]] with
    def useTranslation: Boolean = true
    def getBasis[DDomain[DD] <: DiscreteDomain[DD]](
      model: DiscreteLowRankGaussianProcess[_2D, DDomain, EuclideanVector[_2D]],
      center: Point[_2D]
    ): DenseMatrix[Double] = {
      val np = model.domain.pointSet.numberOfPoints
      val x = centeredP(model.domain, center)

      // derivative of the rotation matrix
      val dr = new DenseMatrix[Double](2, 2, Array(0.0, -1.0, 1.0, 0.0))
      val dx = (dr * x).reshape(2 * np, 1)
      val n = breeze.linalg.norm(dx, breeze.linalg.Axis._0)
      dx / n(0)
    }
