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

import com.sun.tools.javac.code.TypeTag
import scalismo.hdfjson.HDFPath

import scala.util.Try

trait StatisticalModelReader {

  def close(): Unit

  def exists(path: HDFPath): Boolean

  def readString(path: HDFPath): Try[String]

  def readStringAttribute(path: HDFPath, attrName: String): Try[String]

  def readIntAttribute(path: HDFPath, attrName: String): Try[Int]

  def getPathOfChildren(path: HDFPath): Try[Seq[HDFPath]]

  def readNDArrayFloat(path: HDFPath): Try[NDArray[Float]]

  def readNDArrayInt(path: HDFPath): Try[NDArray[Int]]

  def readArrayFloat(path: HDFPath): Try[Array[Float]]
  def readArrayInt(path: HDFPath): Try[Array[Int]]

  def readInt(path: HDFPath): Try[Int]

  def readFloat(path: HDFPath): Try[Float]

}
