package org.statismo.stk.core.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import org.statismo.stk.core.statisticalmodel.{ASMProfileDistributions, MultivariateNormalDistribution, ActiveShapeModel}
import org.statismo.stk.core.common.UnstructuredPointsDomain
import org.statismo.stk.core.geometry.{Point, ThreeD, Point3D}
import breeze.linalg.{DenseMatrix, DenseVector}
import scala.util.{Try, Success}
import org.statismo.stk.core.numerics.FixedPointsUniformMeshSampler3D
import org.statismo.stk.core.image.ContinuousScalarImage
import org.statismo.stk.core.mesh.TriangleMesh
import ncsa.hdf.`object`.Group

/**
 * Created by Luethi on 09.03.14.
 */
class ActiveShapeModelIOTests  extends FunSpec with ShouldMatchers {

  org.statismo.stk.core.initialize()

  private def createTmpH5File() : File = {
    val f= File.createTempFile("hdf5file", ".h5")
    f.deleteOnExit()
    f
  }


  class FeatureExtractor() extends ActiveShapeModel.FeatureExtractor {
    def apply(img : ContinuousScalarImage[ThreeD], t : TriangleMesh, p : Point[ThreeD]) : DenseVector[Float] = {
      DenseVector.zeros[Float](1)
    }
  }

  implicit val featureExtractorHDF5Serializer = new HDF5ReadWrite[FeatureExtractor] {

    override def write(value : FeatureExtractor, group : Group) : Try[Unit] = {
      Success(())
    }
    override def read(group : Group) : Try[FeatureExtractor] = {
      Success(new FeatureExtractor())
    }
  }


  private def createASM : ActiveShapeModel[FeatureExtractor] = {
    val statismoFile = new File(getClass().getResource("/facemodel.h5").getPath())
    val shapeModel = StatismoIO.readStatismoMeshModel(statismoFile).get

    val (profilePoints, _) = (new FixedPointsUniformMeshSampler3D(shapeModel.mesh, 100, 42)).sample.unzip
    val ptDomain = new UnstructuredPointsDomain[ThreeD](profilePoints)
    val dists = for (i <- 0 until ptDomain.numberOfPoints) yield
      (new MultivariateNormalDistribution(DenseVector.ones[Float](3) * i.toFloat , DenseMatrix.eye[Float](3) * i.toFloat))
    val profileDists = ASMProfileDistributions(ptDomain, dists.toArray)
    new ActiveShapeModel(shapeModel,  profileDists, new FeatureExtractor())
  }

  describe("An active shape model") {

    it("can be written to disk and read again") {
      val originalASM = createASM

      val h5file = new File("d:\\temp\\x.h5")//createTmpH5File()

      val statusWrite = for {
        _ <- ActiveShapeModelIO.writeASM(originalASM, h5file)
      } yield ()

      statusWrite.get // throw error if it occured

      // read it again
      val newAsmOrError = for {
        asm <- ActiveShapeModelIO.readASM(h5file)
      } yield asm

      val newASM = newAsmOrError.get
      newASM.intensityDistributions should equal(originalASM.intensityDistributions)

    }

  }

}
