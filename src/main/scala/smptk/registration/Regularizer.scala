package smptk.registration

import scala.language.higherKinds


import TransformationSpace.ParameterVector

trait Regularizer extends (ParameterVector => Float) { }

object Regularizer {

}