package scalismo.io.experimental

import java.io.File

import scalismo.statisticalmodel.experimental.StatisticalVolumeMeshModel

import scala.util.Try

object StatisticalModelIO {

  /**
   * Reads a statistical mesh volume model. The file type is determined
   * based on the extension. Currently on the Scalismo format (.h5)
   * is supported.
   *
   * @param file The statismo file
   * @return A StatisticalVolMeshModel or the Failure
   */
  def readStatisticalVolumeMeshModel(file: File): Try[StatisticalVolumeMeshModel] = {
    // currently, we support only the statismo format
    StatismoIO.readStatismoVolumeMeshModel(file, "/")
  }

  /**
   * Write a statistical mesh volume model.
   * Currently on the Scalismo format (.h5) is supported.
   *
   * @param model The volume mesh model
   * @param file  The statismo file
   * @return A StatisticalMeshVolumeModel or the Failure
   */
  def writeStatisticalVolumeMeshModel(model: StatisticalVolumeMeshModel, file: File): Try[Unit] = {
    // currently, we support only the statismo format
    StatismoIO.writeStatismoVolumeMeshModel(model, file, "/")
  }
}
