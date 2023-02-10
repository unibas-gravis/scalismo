package scalismo.io.statisticalmodel

import io.jhdf.HdfFile

import java.io.Closeable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.Try

class HDF5Reader(h5file: HdfFile) extends Closeable with StatisticalModelReader {

  override def close(): Unit = { h5file.close() }

  def exists(path: String): Boolean = Try { h5file.getByPath(path) }.isSuccess

  def readString(path: String): Try[String] = {

    Try {
      h5file.getDatasetByPath(sanitizePath(path)).getData.asInstanceOf[String]
    }
  }

  private def sanitizePath(path: String): String = {
    val pathWithoutTrailingSlash = if (path.endsWith("/")) {
      path.dropRight(1)
    } else {
      path
    }
    pathWithoutTrailingSlash.replaceAll("//", "/")
  }

  def readStringAttribute(path: String, attrName: String): Try[String] = {
    Try {
      val attribute = h5file
        .getByPath(sanitizePath(path))
        .getAttribute(attrName)
        .getData()

      if (attribute.isInstanceOf[String])
        attribute.asInstanceOf[String]
      else attribute.asInstanceOf[Array[String]].head
    }
  }

  def readIntAttribute(path: String, attrName: String): Try[Int] = {

    Try {
      val attribute = h5file
        .getByPath(sanitizePath(path))
        .getAttribute(attrName)
        .getData()

      if (attribute.isInstanceOf[Int])
        attribute.asInstanceOf[Int]
      else attribute.asInstanceOf[Array[Int]].head
    }

  }

  def getPathOfChildren(path: String): Try[Seq[String]] = {
    Try {
      h5file
        .getByPath(sanitizePath(path))
        .asInstanceOf[io.jhdf.api.Group]
        .getChildren
        .asScala
        .keys
        .toSeq
    }
  }

  /**
   * Reads an ndArray from the path. The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readNDArray[T](path: String)(implicit dataCast: ObjectToArrayCast[T]): Try[NDArray[T]] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      val dims = node.getDimensions

      val array = dataCast.cast(node.getData());
      NDArray(dims.map(_.toLong).toIndexedSeq, array)
    }
  }

  /*
   * Reads an Array from the path.
   * The dataCast is a caster (usually done with asInstance[T])
   * which casts a type Object into an Array[T]. The reason this has to be provided is that
   * it is not possible to cast to a generic type, due to type erasure.
   */
  def readArray[T](path: String)(implicit dataCast: ObjectToArrayCast[T]): Try[Array[T]] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      val dims = node.getDimensions
      dataCast.cast(node.getData());
    }
  }

  def readInt(path: String): Try[Int] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      node.getData().asInstanceOf[Int]
    }
  }

  def readFloat(path: String): Try[Float] = {
    Try {
      val node = h5file.getDatasetByPath(sanitizePath(path))
      node.getData().asInstanceOf[Float]
    }
  }

  def getGroup(path: String): Try[io.jhdf.api.Group] = Try {
    h5file.getByPath(sanitizePath(path)).asInstanceOf[io.jhdf.api.Group]
  }

  def getGroup(group: io.jhdf.api.Group, groupName: String): Try[io.jhdf.api.Group] = Try {
    h5file.getByPath(sanitizePath(group.getPath + "/" + groupName)).asInstanceOf[io.jhdf.api.Group]
  }
}
