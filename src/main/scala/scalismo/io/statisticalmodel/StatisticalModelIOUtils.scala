package scalismo.io.statisticalmodel

import io.jhdf.HdfFile
import ncsa.hdf.`object`.FileFormat

import java.io.{File, IOException}
import scala.util.Try

// make it a proper class and wrapper around the object
object StatisticalModelIOUtils {

  // map untyped FileAccessModel of HDF5 (which is just a string)
  // to typed values
  object FileAccessMode extends Enumeration {
    type FileAccessMode = Value
    val WRITE, CREATE = Value
  }
  import FileAccessMode._

  def hdf5Version = "to be defined"

  private def openFileWriterOrCreateFile(file: File, mode: FileAccessMode): Try[HDF5Writer] = Try {
    val filename = file.getAbsolutePath
    val h5fileAccessMode = mode match {
      case WRITE  => FileFormat.WRITE
      case CREATE => FileFormat.CREATE
    }

    val fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5)
    val h5file = fileFormat.createInstance(filename, h5fileAccessMode)

    if (h5file.open() == -1) {
      throw new IOException("could not open file " + file.getAbsolutePath)
    }
    new HDF5Writer(h5file)
  }

  def openFileForReading(file: File): Try[StatisticalModelReader] = {
    Try {
      val hdfFile = new HdfFile(file)
      new HDF5Reader(hdfFile)
    }
  }

  def openFileForWriting(file: File): Try[HDF5Writer] = openFileWriterOrCreateFile(file, WRITE)
  def createFile(file: File): Try[HDF5Writer] = openFileWriterOrCreateFile(file, CREATE)

}
