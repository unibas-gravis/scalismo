package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.scalatest.FailureMessages
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException
import org.statismo.stk.core.geometry._
import org.statismo.stk.core.geometry.implicits._
import org.statismo.stk.core.io.RegistrationResultsIO._
import org.statismo.stk.core.registration.TransformationSpace.ParameterVector
import org.statismo.stk.core.registration.RotationSpace2D
import play.api.libs.json.Json
import breeze.linalg.DenseVector
import org.statismo.stk.core.registration.RegistrationResult
import org.statismo.stk.core.registration.RegistrationConfiguration
import org.statismo.stk.core.numerics.GradientDescentOptimizer
import org.statismo.stk.core.numerics.Integrator
import org.statismo.stk.core.numerics.GradientDescentConfiguration
import org.statismo.stk.core.registration.MeanSquaresMetric2D
import org.statismo.stk.core.registration.RKHSNormRegularizer
import org.statismo.stk.core.registration.TranslationSpace2D
import org.statismo.stk.core.numerics.IntegratorConfiguration
import org.statismo.stk.core.numerics.UniformDistributionRandomSampler2D
import org.statismo.stk.core.common.BoxedDomain2D
import org.statismo.stk.core.registration.RegistrationResult

class RegistrationResultIOTest extends FunSpec with ShouldMatchers {

  describe("RegistrationResultIO") {

    it("can write and read a parameter vector ") {
      val p: ParameterVector = DenseVector[Float](56, 34, 45)
      val str = Json.toJson(p)
      val p2 = Json.fromJson[ParameterVector](str)(parameterVectorRead).get
      assert(p.toArray.deep === p2.toArray.deep)

    }

    it("can write and read a single registrationResult") {

      val transform = RotationSpace2D(Point2D(0, 0))(DenseVector(5))
      val regResult = RegistrationResult(transform, DenseVector(5))
      val str = Json.toJson(regResult)
      val readRegResult = Json.fromJson[ParameterVector](str)(regResultRead).get
      assert(readRegResult.toArray.deep === DenseVector(5).toArray.deep)
    }

    val space1 = RotationSpace2D(Point2D(0, 0))
    val space2 = TranslationSpace2D()
    val domain = BoxedDomain2D(Point2D(0, 0), Point2D(10, 10))

    val params1 = DenseVector(5f)
    val params2 = DenseVector(3f, 4f)

    val regResult1 = RegistrationResult(space1(params1), params1)
    val regResult2 = RegistrationResult(space2(params2), params2)

    val confFilePath = getClass().getResource("/registrationConfig.scala").getPath();

    it("can write and read a list of registrationResults") {

      val str = regResult2DListWrite.writes((List(regResult1, regResult2), confFilePath))

      val readRegResult = Json.fromJson[RegistrationResult[TwoD]](str)(regResult2DListRead).get

      val p = Point2D(15, 78)
      val readTransformedPoint = readRegResult.transform(p)
      val correctTransformedPoint = (space1(params1) compose space2(params2))(p)

      assert(readTransformedPoint(0) === correctTransformedPoint(0))
      assert(readTransformedPoint(1) === correctTransformedPoint(1))
    }

    it("handles mismatching list sizes of regResult and regConfig objects") {
    	val space3 = TranslationSpace2D()
    	val regResult3 = RegistrationResult(space3(params2), params2)
    	
    	val str = regResult2DListWrite.writes((List(regResult1, regResult2, regResult3), confFilePath))
    	
    	val thrown = intercept[Exception] {
    		Json.fromJson[RegistrationResult[TwoD]](str)(regResult2DListRead)
    	}

    	assert(thrown.getMessage.size>0)
    }
  }
}