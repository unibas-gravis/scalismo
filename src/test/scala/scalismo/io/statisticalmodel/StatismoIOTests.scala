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

  describe("the StatismoIO methods") {}

  it("can write and read again a domain without cells") {

    val upd = UnstructuredPointsDomain(
      IndexedSeq(
        Point3D(0,1,2),
      )
    )

    val tmpDir = Files.createTempDirectory("test-StatismoIO")
    tmpDir.toFile.deleteOnExit()
    val dummyFile = new File(tmpDir.toFile, "empty-representer.h5.json")

    val modelPath = HDFPath("/")
    val representerPath = HDFPath(modelPath,"representer")

    val privateMethodWriter = PrivateMethod[Try[Unit]](Symbol("writeRepresenterStatismov090"))
    val privateMethodReader = PrivateMethod[Try[UnstructuredPointsDomain[_3D]]](Symbol("readStandardMeshRepresentation"))

    val t = for {
      h5out <- StatisticalModelIOUtils.openFileForWriting(dummyFile)
      t <- StatismoIO invokePrivate privateMethodWriter(h5out, representerPath, upd, modelPath, scalismo.geometry.Dim.ThreeDSpace, scalismo.io.StatismoDomainIO.domainIOUnstructuredPoints3D)
      _ <- h5out.write()
      _ <- Try {
        h5out.close()
      }
      h5file <- StatisticalModelIOUtils.openFileForReading(dummyFile)
      loadedRefMesh <- StatismoIO invokePrivate privateMethodReader(h5file, modelPath, scalismo.geometry.Dim.ThreeDSpace, scalismo.io.StatismoDomainIO.domainIOUnstructuredPoints3D, scalismo.geometry.EuclideanVector.Vector_3DVectorizer)
    } yield {
      assertModelAlmostEqual(upd, loadedRefMesh)
    }
    t.get

  }
}
