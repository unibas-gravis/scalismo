package scalismo

import java.io.File

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{PointId, UnstructuredPointsDomain}
import scalismo.geometry.{EuclideanVector3D, Point, Point3D, _3D}
import scalismo.image.DiscreteScalarImage
import scalismo.io.TetraMeshIO
import scalismo.tetramesh._
import vtk.vtkTetra


object main {
  def main(args: Array[String]): Unit = {
    scalismo.initialize()

    val tetra = TetraMeshIO.readTetraMesh(new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\tetra.vtu")).get
    //val tetra = TetraMeshIO.readTetraMesh(new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\humerusvolume.inp")).get

    println(tetra.isInsideTetrahedralCell(Point(-0.5,0.5,0.05),tetra.tetrahedrons.apply(0)))
    println(tetra.volume)

    println(tetra.pointSet.findClosestPoint(Point(-1.0,0.0,1.1)))





    val pts = IndexedSeq(Point(0.5,-1.3,1.0), Point(-1.0,0.0,1.1), Point(0.0,1.0,-1.0), Point(0.6,1.0,0.0))
    val cells = IndexedSeq(TetrahedralCell(PointId(0), PointId(1), PointId(2), PointId(3)))
   val tetramesh = TetrahedralMesh3D(UnstructuredPointsDomain(pts), TetrahedralList(cells))
    TetraMeshIO.writeTetraMesh(tetramesh, new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\tetra.vtu"))




  }

  /**
    *  this function eturns the a sequence of volex with their corresponding pixel value  of a given tetrahedrons
    *
    *
    */

  def GettetrahedronVoxels(tetra: TetrahedralMesh[_3D],t:TetrahedralCell,image: DiscreteScalarImage[_3D,Double])={

    val seq= Seq((Point3D(0.0,0.0,0.0),0.0))
    val l=image.pointsWithValues.map{pid=> if (tetra.isInsideTetrahedralCell(pid._1,t)) {seq++Seq((pid._1,pid._2));pid._1}else 0}


    for {p<- seq if ((p._1.x!=0.0)&&(p._1.y!=0.0)&&(p._1.z!=0.0))} yield{
      p
    }

  }


  /**
    *  this function eturns the a sequence berstein polynomial coeficient of tetrahedrons (defining the tetrahedral mesh)
    * the densevector at the index i contains the poly coef of the tetrahedron (cell) at the index i.
    *
    *
    */
  def GetTetrahedralMeshIntensityFromImage(tetra: TetrahedralMesh[_3D], image: DiscreteScalarImage[_3D,Double], degree:Int)={

    val s=tetra.tetrahedrons.map{t=>(GettetrahedronVoxels(tetra,t,image),
      Seq(tetra.pointSet.point(t.ptId1),tetra.pointSet.point(t.ptId2),tetra.pointSet.point(t.ptId3),tetra.pointSet.point(t.ptId4)))}

    s.map{sq=>density_fuction(sq._1,sq._2,degree)}


  }



  /**
   this function give the value of the baricentric Bernstein basis for a given baricentric coordinates
   it works by calulating B^{n}_{i,j,k,l}(u=(u_x,u_y,u_z,u_w))=n!/(i!j!k!l!)u^i_xu^j_yu^k_zu^l_w

    * u=(u_x,u_y,u_z,u_w) barentric coordinates
    * n degree of the Bernstein polynomial
    *
    *
    *
    */
  def Bernstein_ijkl(n:Int,i:Int,j:Int,k:Int,l:Int,u:DenseVector[Double]):Double ={
    (factorial(n)*(1.0/(factorial(i)*factorial(j)*factorial(k)
      *factorial(l))))*Math.pow(u(0),i)*Math.pow(u(1),j)*Math.pow(u(2),k)*Math.pow(u(3),l)
  }

  //this gives foctoriel of n
  def factorial(n: Int): Int = {

    var f = 1
    for(i <- 1 to n)
    {
      f = f * i;
    }

    return f
  }


  /**this function compute the coeficient of the desnsity function of a tetrahehron

 * tetrahedron: Seq[Point[_3D],  sequence of point defining the tetrahedron
  *seqvoxelPixel: Seq[(Point[_3D],Double)], sequence of voxel inside the tetrahedron
      and their corresponding pivel values
 * n: degree of berstein polynome
   */
  def PermutList(n:Int):Seq[DenseVector[Int]]={
    var seqvec=Seq(DenseVector.zeros[Int](4))
    for (j<-0 to n){
      for (k<-0 to n){
        for (l<-0 to n){
          if (j+k+l<=n){
            seqvec=seqvec++Seq(DenseVector[Int](n-j-k-l,j,k,l))
          }
        }
      }
    }

    seqvec
  }


  /**
    this function compute the coeficient of the desnsity function of a tetrahehron
    * tetrahedron: Seq[Point[_3D],  sequence of point defining the tetrahedron
    * seqvoxelPixel: Seq[(Point[_3D],Double)], sequence of voxel inside the tetrahedron
    * and their corresponding pivel values
    * n: degree of berstein polynome
    *

    */
  def density_fuction(seqvoxelPixel:Seq[(Point[_3D],Double)],tetrahedron:Seq[Point[_3D]],degpol:Int):DenseVector[Double]={

    val list=PermutList(degpol)

    var M=DenseMatrix.zeros[Double](seqvoxelPixel.size,list.size-1)
    var T=DenseVector.zeros[Double](seqvoxelPixel.size)

    for (j<-0 to seqvoxelPixel.size-1){
      var v=DenseVector.zeros[Double](list.size-1)
      for (i<-1 to list.size-1){
        v(j)=Bernstein_ijkl(degpol,list.apply(i)(0),list.apply(i)(1),list.apply(i)(2),
          list.apply(i)(3),seqvoxelPixel.apply(j)._1.toBreezeVector)

      }
      T(j)=seqvoxelPixel.apply(j)._2

      M(j,::):=v.t
    }

    val SVD(u, d2, vt) = breeze.linalg.svd(M)

    val Dinv = d2.map(d => if (d > 1e-6) 1.0 / d else 0.0)
    val x=(vt.t*breeze.linalg.diag(Dinv)*u.t)*T

    x
  }







}
