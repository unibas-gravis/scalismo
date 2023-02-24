/*
 * Copyright 2023 University of Basel, Graphics and Vision Research Group
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
package scalismo.io.statisticalmodel

trait ObjectToArrayCast[A] {
  def cast(arr: Object): Array[A]

}

object ObjectToArrayCast {
  implicit object ObjectToStringArrayCast extends ObjectToArrayCast[String] {
    override def cast(arr: Object): Array[String] = {
      if (arr.isInstanceOf[Array[Array[String]]]) {
        arr.asInstanceOf[Array[Array[String]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[String]]]]) {
        arr.asInstanceOf[Array[Array[Array[String]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[String]]
      }
    }
  }

  implicit object ObjectToFloatArrayCast extends ObjectToArrayCast[Float] {
    override def cast(arr: Object): Array[Float] = {
      if (arr.isInstanceOf[Array[Array[Float]]]) {
        arr.asInstanceOf[Array[Array[Float]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Float]]]]) {
        arr.asInstanceOf[Array[Array[Array[Float]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Float]]
      }
    }
  }

  implicit object ObjectToByteArrayCast extends ObjectToArrayCast[Byte] {
    override def cast(arr: Object): Array[Byte] = {
      if (arr.isInstanceOf[Array[Array[Byte]]]) {
        arr.asInstanceOf[Array[Array[Byte]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Byte]]]]) {
        arr.asInstanceOf[Array[Array[Array[Byte]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Byte]]
      }
    }
  }

  implicit object ObjectToIntArrayCast extends ObjectToArrayCast[Int] {
    override def cast(arr: Object): Array[Int] = {
      if (arr.isInstanceOf[Array[Array[Int]]]) {
        arr.asInstanceOf[Array[Array[Int]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Int]]]]) {
        arr.asInstanceOf[Array[Array[Array[Int]]]].flatten.flatten
      } else {
        if (arr.isInstanceOf[Array[Array[Long]]])
          arr.asInstanceOf[Array[Array[Long]]].flatten.map(_.toInt)
        else
          arr.asInstanceOf[Array[Int]]
      }
    }
  }

  implicit object ObjectToDoubleArrayCast extends ObjectToArrayCast[Double] {
    override def cast(arr: Object): Array[Double] = {
      if (arr.isInstanceOf[Array[Array[Double]]]) {
        arr.asInstanceOf[Array[Array[Double]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Double]]]]) {
        arr.asInstanceOf[Array[Array[Array[Double]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Double]]
      }
    }
  }
}
