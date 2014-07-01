package org.statismo.stk.core.geometry

import scala.language.implicitConversions
import breeze.linalg.DenseVector
import scala.reflect.ClassTag
import breeze.linalg.DenseMatrix

/** a marker trait to distinguish the dimension */
sealed trait Dim
trait OneD extends Dim
trait TwoD extends Dim
trait ThreeD extends Dim






