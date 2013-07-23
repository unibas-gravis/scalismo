package smptk
package io

import java.io.File
import mesh.TriangleMesh
import scala.util.Try
import mesh.TriangleCell
import mesh.TriangleMesh
import smptk.mesh.TriangleMeshDomain
import smptk.geometry._

object MeshIO {

  
  def writeHDF5(surface: TriangleMesh, file: File): Try[Unit] = {
    val h5file = HDF5Utils.createFile(file)
    val domainPoints : IndexedSeq[Point[ThreeD]] = surface.domain.points.toIndexedSeq
    val cells : IndexedSeq[TriangleCell] = surface.domain.cells
    
    val maybeError: Try[Unit] = for {
      _ <- h5file.writeNDArray("/Surface/0/Vertices", pointSeqToNDArray(domainPoints))
      _ <- h5file.writeNDArray("/Surface/0/Cells", cellSeqToNDArray(cells))
    } yield {()}
    h5file.close()
    maybeError
  }
  
  def readHDF5(file: File): Try[TriangleMesh] = { 

    val filename = file.getAbsolutePath()
    val h5file = HDF5Utils.openFileForReading(file)

    val maybeSurface = for {    
      vertArray <- h5file.readNDArray[Double]("/Surface/0/Vertices")
      cellArray <- h5file.readNDArray[Int]("/Surface/0/Cells")      
    } yield TriangleMesh(TriangleMeshDomain(NDArrayToPointSeq(vertArray), NDArrayToCellSeq(cellArray)))

    h5file.close()
    maybeSurface
  }
  
  private def NDArrayToPointSeq(ndarray : NDArray[Double]) : IndexedSeq[Point3D] =  {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq 
	ndarray.data.grouped(3).map(grp => Point3D(grp(0), grp(1), grp(2))).toIndexedSeq
  }

private def NDArrayToCellSeq(ndarray : NDArray[Int]) : IndexedSeq[TriangleCell] =  {
    // take block of 3, map them to 3dPoints and convert the resulting array to an indexed seq 
	ndarray.data.grouped(3).map(grp => TriangleCell(grp(0), grp(1), grp(2))).toIndexedSeq
  }

  private def pointSeqToNDArray[T](points : IndexedSeq[Point[ThreeD]]) : NDArray[Double] = 
    NDArray(Vector(points.size, 3), points.flatten(pt => pt.data).toArray)
  
    private def cellSeqToNDArray[T](cells : IndexedSeq[TriangleCell]) : NDArray[Int] = 
      NDArray(Vector(cells.size, 3), cells.flatten(cell => cell.pointIds).toArray)
}