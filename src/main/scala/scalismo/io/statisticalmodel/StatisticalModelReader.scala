package scalismo.io.statisticalmodel

import scala.util.Try


trait StatisticalModelReader {

  def close(): Unit

  def exists(path: String): Boolean

  def readString(path: String): Try[String]

  def readStringAttribute(path: String, attrName: String): Try[String]

  def readIntAttribute(path: String, attrName: String): Try[Int]

  def getPathOfChildren(path: String): Try[Seq[String]]

  /**
   * Reads an ndArray from the path. The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readNDArray[T](path: String)(implicit dataCast: ObjectToArrayCast[T]): Try[NDArray[T]]

  /*
   * Reads an Array from the path.
   * The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readArray[T](path: String)(implicit dataCast: ObjectToArrayCast[T]): Try[Array[T]]

  def readInt(path: String): Try[Int]

  def readFloat(path: String): Try[Float]

}