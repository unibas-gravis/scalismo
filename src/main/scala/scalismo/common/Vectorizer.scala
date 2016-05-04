package scalismo.common

import breeze.linalg.DenseVector

/**
 * Created by forand00 on 03.05.16.
 */
trait Vectorizer[Value] {
  def dim: Int

  def vectorize(v: Value): DenseVector[Double]

  def unvectorize(d: DenseVector[Double]): Value

}
