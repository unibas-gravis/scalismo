package org.statismo.stk.core.io

import play.api.libs.json._
import play.api.libs.functional._
import play.api.libs.functional.syntax._
import breeze.linalg.DenseVector
import play.api.data.validation.ValidationError
import play.api.libs.json.Writes._
import org.statismo.stk.core.registration.RegistrationResult
import org.statismo.stk.core.geometry.TwoD
import org.statismo.stk.core.registration.TransformationSpace.ParameterVector
import org.statismo.stk.core.geometry.ThreeD
import com.twitter.util.Eval
import org.statismo.stk.core.registration.RegistrationConfiguration
import java.io.File

object RegistrationResultsIO {

  //def notEqualReads[Array[Double]](v: Int)(implicit r: Reads[Array[Double]]): Reads[Array[Double]] = Reads.filterNot(ValidationError("validate.error.unexpected.value", v))( p=> p == v )
  implicit val parameterVectorRead = ((__ \ "parameterVector").read[Array[Float]]).map(DenseVector.apply _)

  implicit object parameterVectorWrite extends Writes[ParameterVector] {
    def writes(m: ParameterVector) = JsObject(List("parameterVector" -> Json.toJson(m.toArray)))
  }
  
  implicit object regResult2DWrite extends Writes[RegistrationResult[TwoD]] {
    def writes(m: RegistrationResult[TwoD]) = JsObject(List("TransformParameters" -> Json.toJson(m.parameters), "configFileName" -> Json.toJson(m.configurationFileName) ))
  }
  
  implicit object regResult3DWrite extends Writes[RegistrationResult[ThreeD]] {
    def writes(m: RegistrationResult[ThreeD]) = JsObject(List("TransformParameters" -> Json.toJson(m.parameters), "configFileName" -> Json.toJson(m.configurationFileName) ))
  }
  
  implicit val regResult2DRead =((__ \ "TransformParameters").read[ParameterVector] and (__ \ "configFileName").read[String])( (params, fileName) => {
    val config = Eval[RegistrationConfiguration[TwoD]](new File(fileName)) // this could be very dangerous since implicit execution of config code
    RegistrationResult(config.transformationSpace(params), params)
  })
  
  implicit val regResult3DRead =((__ \ "TransformParameters").read[ParameterVector] and (__ \ "configFileName").read[String])( (params, fileName) => {
    val config = Eval[RegistrationConfiguration[ThreeD]](new File(fileName)) // this could be very dangerous since implicit execution of config code
    RegistrationResult(config.transformationSpace(params), params, Some(fileName))
  })
}