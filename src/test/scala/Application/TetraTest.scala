package Application

import scalismo.common.NearestNeighborInterpolator
import scalismo.geometry.{ EuclideanVector, Point, _3D }
import scalismo.io.MeshIO
import scalismo.kernels.{ DiagonalKernel, GaussianKernel }
import scalismo.statisticalmodel.{ GaussianProcess, LowRankGaussianProcess, StatisticalVolumeMeshModel }
import scalismo.tetramesh.{ ScalarVolumeMeshField, TetrahedralMesh }

object TetraTest {

  scalismo.initialize()
  implicit val rng = scalismo.utils.Random(42L)

  def main(args: Array[String]): Unit = {
    val referenceMesh: TetrahedralMesh[_3D] = MeshIO.readTetrahedralMesh(new java.io.File("./src/test/resources/TetraMesh.vtk")).get

    // Creating a GP
    val gk = DiagonalKernel[_3D](GaussianKernel(10), 3)
    val gp = GaussianProcess[_3D, EuclideanVector[_3D]](gk)
    val lowRankGP = LowRankGaussianProcess.approximateGPCholesky(referenceMesh.pointSet, gp, 1e-5, NearestNeighborInterpolator())

    // Creating a statistical volume model
    val volumeModel = StatisticalVolumeMeshModel(referenceMesh, lowRankGP)

    // Sampling from the models
    for (i <- 0 until 10) {
      val sampleVolume = volumeModel.sample()
      MeshIO.writeTetrahedralMesh(sampleVolume, new java.io.File(s"sample-$i.vtk")).get
    }

    // Creating scalar representations
    val scalars = referenceMesh.pointSet.points.map((pt: Point[_3D]) => 1.0).toIndexedSeq
    val volumeMeshField = ScalarVolumeMeshField(referenceMesh, scalars)
    val volumeField = volumeMeshField.interpolate(NearestNeighborInterpolator())

    // Warping a scalar volume mesh with the output of a GP
    val sampleDeformation = lowRankGP.sample()
    val warpedVolumeMeshField = volumeMeshField.transform(p => p + sampleDeformation(p))

    // val ui = ScalismoUI()
    // ui.show(volumeMeshField, "volumeMesh")
  }

}
