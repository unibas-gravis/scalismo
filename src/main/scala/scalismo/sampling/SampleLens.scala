package scalismo.sampling

import breeze.linalg.DenseVector

/**
 * This trait is used in the context of an MCMC chain, where the we have a class Sample or
 * generic type A, which contains an arbitrary number of parameters, with arbitrary structure.
 * The lens focuses attention to one part of the parameter vector, which can then be read and updated
 * using the provided getter and setter methods.
 * To update different parts of a sample, different lenses must be implemented
 */
trait SampleLens[A] {

  /**
   * The number of parameters this lense focuses on
   */
  def numberOfParameters: Int

  /**
   * returns the part of the parameters on which this lense focuses as a dense vector
   * The length of the vectors needs to correspond to numberOfParameters
   */
  def getAsVector(sample: A): DenseVector[Double]

  /**
   * returns a new sample, which the part, on which this lense focuses replaced by the values
   * given in the vector. The option generatedBy argument is used to tag who has generated
   * the resulting sample.
   *
   * The length of the given vectors needs to correspond to numberOfParameters
   */
  def setFromVector(sample: A, vector: DenseVector[Double], generatedBy: Option[String] = None): A
}
