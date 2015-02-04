package scalismo.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.SpatiallyIndexedFiniteDiscreteDomain
import scalismo.image.ScalarImage
import scalismo.numerics.FixedPointsUniformMeshSampler3D
import scalismo.statisticalmodel.{ASMProfileDistributions, MultivariateNormalDistribution, ActiveShapeModel}
import scalismo.statisticalmodel.ActiveShapeModel.NormalDirectionFeatureExtractor
import scala.util.{Try, Success}
import ncsa.hdf.`object`.Group

/**
 * Created by Luethi on 09.03.14.
 */
class ActiveShapeModelIOTests  extends FunSpec with ShouldMatchers {

  scalismo.initialize()

  private def createTmpH5File() : File = {
    val f= File.createTempFile("hdf5file", ".h5")
    f.deleteOnExit()
    f
  }



  private def createASM : ActiveShapeModel[NormalDirectionFeatureExtractor] = {
    val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
    val shapeModel = StatismoIO.readStatismoMeshModel(statismoFile).get

    val (profilePoints, _) = (new FixedPointsUniformMeshSampler3D(shapeModel.referenceMesh, 100, 42)).sample.unzip
    val ptDomain = SpatiallyIndexedFiniteDiscreteDomain.fromSeq(profilePoints)
    val dists = for (i <- 0 until ptDomain.numberOfPoints) yield
      (new MultivariateNormalDistribution(DenseVector.ones[Float](3) * i.toFloat , DenseMatrix.eye[Float](3) * i.toFloat))
    val profileDists = ASMProfileDistributions(ptDomain, dists)
    new ActiveShapeModel(shapeModel,  profileDists, new NormalDirectionFeatureExtractor(5, 10))
  }

  describe("An active shape model") {

    it("can be written to disk and read again") {
      val originalASM = createASM

      val h5file = createTmpH5File()

      val statusWrite = for {
        _ <- ActiveShapeModelIO.writeASM(originalASM, h5file)
      } yield ()

      statusWrite.get // throw error if it occured

      // read it again
      val newAsmOrError = for {
        asm <- ActiveShapeModelIO.readASM[NormalDirectionFeatureExtractor](h5file)
      } yield asm

      val newASM = newAsmOrError.get
      newASM.intensityDistributions should equal(originalASM.intensityDistributions)

    }

  }

}
