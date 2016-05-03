package scalismo.common

import breeze.linalg.DenseVector

/**
 * Created by forand00 on 03.05.16.
 */
trait Vectorizer[Value] {
  def dim: Int

  def vectorize(v: Value): DenseVector[Double]

  def unvectorize(d: DenseVector[Double]): Value

  def vectorize(vs: IndexedSeq[Value]): DenseVector[Double] = {
    val fullDim = vs.length * dim
    val M = DenseVector.zeros[Double](fullDim)
    val pt = vs.zipWithIndex
    for (i <- pt) {
      val m = vectorize(i._1)
      for (x <- 0 until dim) {
        M(i._2 * dim + x) = m(x)
      }
    }
    M
  }

  def unvectorizeField(d: DenseVector[Double]): IndexedSeq[Value] = {
    val nElem = d.length / dim
    d.toArray.grouped(dim).map(e => unvectorize(DenseVector(e))).toIndexedSeq
  }

}
