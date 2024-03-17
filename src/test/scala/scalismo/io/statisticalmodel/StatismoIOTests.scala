package scalismo.io.statisticalmodel

import org.scalatest.PrivateMethodTester.*
import scalismo.geometry.{Point3D, _3D}
import scalismo.io.statisticalmodel.StatismoIO
import scalismo.io.StatisticalModelIO
import scalismo.ScalismoTestSuite
import scalismo.common.{UnstructuredPoints, UnstructuredPointsDomain}
import scalismo.hdf5json.HDFPath
import scalismo.mesh.{TriangleList, TriangleMesh, TriangleMesh3D}
import scalismo.statisticalmodel.StatisticalMeshModel

import java.io.File
import java.net.URLDecoder
import java.nio.file.Files
import scala.util.Try

class StatismoIOTests extends ScalismoTestSuite {
  def assertModelAlmostEqual[D](m1: UnstructuredPointsDomain[D], m2: UnstructuredPointsDomain[D]): Unit = {
    m1.pointSet.pointSequence.zip(m2.pointSet.pointSequence).foreach{ case (a,b) =>
      assert((a-b).norm < 1e-5)
    }
  }

  describe("the StatismoIO methods") {

    it("can write and read UnstructuredPointsDomain") {
      val tmpDir = Files.createTempDirectory("test-StatismoIO")
      tmpDir.toFile.deleteOnExit()
      val tmpFile = new File(tmpDir.toFile, "UnstructuredPointsDomain.h5.json")
      tmpFile.deleteOnExit()

      val dataToWrite = UnstructuredPointsDomain(
        IndexedSeq(
          Point3D(0, 1, 2),
        )
      )

      val modelPath = HDFPath("/")
      val representerPath = HDFPath(modelPath, "representer")

      // helper to test / call private methods
      val writerMethod = PrivateMethod[Try[Unit]](Symbol("writeRepresenterStatismov090"))
      val readerMethod = PrivateMethod[Try[UnstructuredPointsDomain[_3D]]](Symbol("readStandardMeshRepresentation"))
      val ndSpace = scalismo.geometry.Dim.ThreeDSpace
      val domainIO = scalismo.io.StatismoDomainIO.domainIOUnstructuredPoints3D
      val vectorizer = scalismo.geometry.EuclideanVector.Vector_3DVectorizer

      val t = for {
        h5Out <- StatisticalModelIOUtils.openFileForWriting(tmpFile)
        t <- StatismoIO invokePrivate writerMethod(h5Out, representerPath, dataToWrite, modelPath, ndSpace, domainIO)
        _ <- h5Out.write()
        _ <- Try {
          h5Out.close()
        }
        h5In <- StatisticalModelIOUtils.openFileForReading(tmpFile)
        loaded <- StatismoIO invokePrivate readerMethod(h5In, modelPath, ndSpace, domainIO, vectorizer)
      } yield {
        assertModelAlmostEqual(dataToWrite, loaded)
      }

      t.get
    }
  }
}
