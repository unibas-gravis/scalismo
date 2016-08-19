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
package scalismo.numerics

import breeze.linalg.qr.QR
import breeze.linalg.svd.SVD
import breeze.linalg.{ diag, DenseMatrix, DenseVector, norm }
import scalismo.utils.{ Random, Benchmark }

/**
 * Implementation of a Randomized approach for SVD,
 * as proposed in
 * Finding structure with randomness: Probabilistic algorithms for constructing approximate matrix decompositions
 * N Halko, PG Martinsson, JA Tropp - SIAM review, 2011 - SIAM
 */
object RandomSVD {

  def computeSVD(A: DenseMatrix[Double], k: Int)(implicit rand: Random): (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {

    require(A.rows == A.cols) // might be removed later (check in Halko paper)

    val q = 5
    val l = k + 5
    val m = A.rows

    val standardNormal = rand.breezeRandomGaussian(0, 1)

    // create a gaussian random matrix
    val Omega = DenseMatrix.zeros[Double](m, l).map(_ => standardNormal.draw())

    var QFull = breeze.linalg.qr.reduced.justQ(A * Omega)
    for (i <- 0 until q) {
      val QFulltilde = breeze.linalg.qr.reduced.justQ(A.t * QFull)
      QFull = breeze.linalg.qr.reduced.justQ(A * QFulltilde)
    }

    val Q = QFull(::, 0 until Math.min(2 * k, QFull.cols))
    val B = Q.t * A

    val SVD(uHat, sigma, vt) = breeze.linalg.svd.reduced(B)
    val U = Q * uHat
    (U(::, 0 until k), sigma(0 until k), vt(0 until k, 0 until k))
  }

}