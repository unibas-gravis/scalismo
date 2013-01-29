package smptk.registration

import TransformationSpace.ParameterVector

trait Regularizer extends (ParameterVector => Float) { }

object Regularizer {

}