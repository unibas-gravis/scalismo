package scalismo.statisticalmodel

import java.io.File

import scalismo.GeneralGaussianField.Adapted.StatisticalShapeModel

//import scalismo.GeneralGaussianField.Adapted.StatisticalShapeModel
import scalismo.common.DiscreteField
import scalismo.geometry.{ Point, Vector, _3D }
import scalismo.io.MeshIO
import scalismo.statisticalmodel.dataset.DataCollection

/**
 * Created by forand00 on 27.04.16.
 */

object TestStatisticalShapeModel {

  def toFields(dc: DataCollection) = {
    dc.dataItems.map { i =>
      val f = (p: Point[_3D]) => i.transformation(p) - p
      val domain = dc.reference.pointSet
      new DiscreteField[_3D, Vector[_3D]](domain, dc.reference.pointSet.points.toIndexedSeq.map(p => f(p)))
    }
  }

  def main(args: Array[String]) {
    scalismo.initialize()

    val meshes = args.map { s =>
      print("Loading " + s + " ...")
      val m = MeshIO.readMesh(new File(s)).get
      println(" Done.")
      m
    }
    println("Loading finished!!!")
    val reference = meshes.head
    val trainingMeshes = meshes.tail
    val collection = DataCollection.fromMeshSequence(reference, trainingMeshes)._1.get
    //    val ssm = StatisticalMeshModel.createUsingPCA(collection).get
    val trainingFields = toFields(collection)
    val ssm = StatisticalShapeModel(reference, trainingFields)

    for (idx <- 0 until 20) {
      print("Draw sample %d ...".format(idx))
      val sample = ssm.sample
      MeshIO.writeMesh(sample, new File("/tmp/sample%05d.vtk".format(idx)))
      println(" Done.")
    }
    println("Sampling finished!!!")
  }
}
