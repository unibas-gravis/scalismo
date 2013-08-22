package org.statismo.stk.core.io

import play.api.libs.json._
import play.api.libs.functional._
import play.api.libs.functional.syntax._
import breeze.linalg.DenseVector
import play.api.data.validation.ValidationError
import play.api.libs.json.Writes._
import play.api.libs.json.Reads._
import org.statismo.stk.core.registration.RegistrationResult
import org.statismo.stk.core.geometry.TwoD
import org.statismo.stk.core.registration.TransformationSpace.ParameterVector
import org.statismo.stk.core.geometry.ThreeD
import com.twitter.util.Eval
import org.statismo.stk.core.registration.RegistrationConfiguration
import java.io.File
import org.statismo.stk.core.registration.RegistrationResult
import org.statismo.stk.core.registration.TranslationSpace2D

object RegistrationResultsIO {

  //def notEqualReads[Array[Double]](v: Int)(implicit r: Reads[Array[Double]]): Reads[Array[Double]] = Reads.filterNot(ValidationError("validate.error.unexpected.value", v))( p=> p == v )
  implicit val parameterVectorRead = ((__ \ "parameterVector").read[Array[Float]]).map(DenseVector.apply _)

  implicit object parameterVectorWrite extends Writes[ParameterVector] {
    def writes(m: ParameterVector) = JsObject(List("parameterVector" -> Json.toJson(m.toArray)))
  }

  implicit object regResult2DWrite extends Writes[RegistrationResult[TwoD]] {
    def writes(m: RegistrationResult[TwoD]) = JsObject(List("TransformParameters" -> Json.toJson(m.parameters)))
  }

  implicit object regResult3DWrite extends Writes[RegistrationResult[ThreeD]] {
    def writes(m: RegistrationResult[ThreeD]) = JsObject(List("TransformParameters" -> Json.toJson(m.parameters)))
  }

  implicit val regResultRead = (__ \ "TransformParameters").read[ParameterVector](parameterVectorRead)

  implicit object regResult2DListWrite extends Writes[(List[RegistrationResult[TwoD]], String)] {
    def writes(m: (List[RegistrationResult[TwoD]], String)) = {
      JsObject(List("RegistrationLevelsResults" -> Json.toJson(m._1), "configFileName" -> Json.toJson(m._2)))
    }
  }

  implicit object regResult3DListWrite extends Writes[(List[RegistrationResult[ThreeD]], String)] {
    def writes(m: (List[RegistrationResult[ThreeD]], String)) = {
      JsObject(List("RegistrationLevelsResults" -> Json.toJson(m._1), "configFileName" -> Json.toJson(m._2)))
    }
  }

  implicit val regResult2DListRead = ((__ \ "RegistrationLevelsResults").read(Reads.list[ParameterVector](regResultRead)) and (__ \ "configFileName").read[String])((regLevelsResults, fileName) => {
    val config = Eval[List[RegistrationConfiguration[TwoD]]](new File(fileName)) // this could be very dangerous since implicit execution of config code  

    if (config.size == regLevelsResults.size) {
      //create the product transfomration space  (the order here)
      val space = config.map(_.transformationSpace).reduce((c1, c2) => c1.product(c2))
      // and the concatenated parameterVector
      val params = regLevelsResults.reduce(DenseVector.vertcat(_, _))
      RegistrationResult[TwoD](space(params), params)
    } else {
      throw(new Exception("Sizes of configuration objects and result parameters are mistmatching"))
    }
  })

  implicit val regResult3DListRead = ((__ \ "RegistrationLevelsResults").read(Reads.list[ParameterVector](regResultRead)) and (__ \ "configFileName").read[String])((regLevelsResults, fileName) => {
    val config = Eval[List[RegistrationConfiguration[ThreeD]]](new File(fileName)) // this could be very dangerous since implicit execution of config code  
    // should check that the sizes of the two lists match
    if (config.size == regLevelsResults.size) {
      //create the product transfomration space  (the order here)
      val space = config.map(_.transformationSpace).reduce((c1, c2) => c1.product(c2))
      // and the concatenated parameterVector
      val params = regLevelsResults.reduce(DenseVector.vertcat(_, _))
      RegistrationResult[ThreeD](space(params), params)
    } else {
        throw(new Exception("Sizes of configuration objects and result parameters are mistmatching"))
    }
  })

}