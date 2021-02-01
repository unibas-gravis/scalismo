package scalismo.sampling

import breeze.linalg.DenseVector

/**
 * This class is used in the context of an MCMC chain, where the we have a class Sample or
 * generic type A, with arbitrary structure.
 * The lens focuses attention to a part of the sample with type B, which can then be read and updated
 * using the provided getter and setter methods.
 * To update different parts of a sample, different lenses must be implemented
 */
abstract class SampleLens[A, B] {

  /**
   * returns the part of the parameters on which this lens focuses
   */
  def get(sample: A): B

  /**
   * returns a new sample, which the part, on which this lens focuses replaced by the values
   * given in the vector. The option generatedBy argument is used to tag who has generated
   * the resulting sample.
   *
   * The length of the given vectors needs to correspond to numberOfParameters
   */
  def replace(full: A, part: B, generatedBy: Option[String] = None): A
}
