package scalismo.common.interpolation

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.geometry.{NDSpace, Point, _3D}
import scalismo.tetramesh.TetrahedralCell
import vtk.vtkTetra


trait BernsteinPolynomialInterpolation[D] {
  def degree: Int
  def coefficient: DenseVector[Double]
}



object BernsteinPolynomialInterpolation{

  def apply[D: NDSpace](VoxelsWithPixelvalues:Seq[(Point[_3D],Short)],tetrahedron:Seq[Point[_3D]],n:Int): Unit ={

    //this computes the list of bernstein  polynomial coef indexes
    def coeficientIndexList (n:Int):IndexedSeq[(Int,Int,Int,Int)]={
      val seqvec= for {j<-0 to n; k<-0 to n; l<-0 to n; if j+k+l<=n} yield{

        (n-j-k-l,j,k,l)

      }
      seqvec

    }



       // this function returns baricentric coordinate of a point inside a tetrahedron

    def baryOfPointIntetrahedron(p:Point[_3D],a:Point[_3D],b:Point[_3D]
                                 ,c:Point[_3D],d:Point[_3D]):DenseVector[Double]= {


      val bcoord = new Array[Double](4)
      //this to initialised the array where the result will be stored
      val tetrahedron = new vtkTetra()
      tetrahedron.BarycentricCoords(p.toArray, a.toArray, b.toArray, c.toArray, d.toArray, bcoord)
      DenseVector[Double](bcoord.apply(0),bcoord.apply(1),bcoord.apply(2),bcoord.apply(3))
    }


    //this gives foctoriel of n
    def factorial(n: Int): Int = {

      if (n == 0) {
        1
      } else {
        var f = 1
        for (i <- 1 to n) {
          f = f * i
        }

        f
      }
    }

   // computes the the baricentric bernstein basic value
    def Bernstein_ijkl(n:Int,list:(Int,Int,Int,Int),u:DenseVector[Double]):Double ={
      (factorial(n)*Math.pow(u(0),list._1)*Math.pow(u(1),list._2)*Math.pow(u(2),list._3)*Math.pow(u(3),list._4))*
        (1.0/(factorial(list._1)*factorial(list._2)*factorial(list._3)*factorial(list._4)))
    }

//the following solve the linear equation in (reference paper: Yao, J. and Taylor, R.H., 2001, July.
// Construction and simplification of bone density models. In Medical Imaging 2001)
    val list=coeficientIndexList(n)

    var M=DenseMatrix.zeros[Double](VoxelsWithPixelvalues.size,list.size)//the -1 here is to remove the sezo vector that was added to list
    var T=DenseVector.zeros[Double](VoxelsWithPixelvalues.size)


    for (j<-0 to VoxelsWithPixelvalues.size-1){
      var v=DenseVector.zeros[Double](list.size)

      val barcenter=baryOfPointIntetrahedron(VoxelsWithPixelvalues.apply(j)._1,tetrahedron.apply(0),tetrahedron.apply(1),tetrahedron.apply(2),tetrahedron.apply(3))
      for (i<-0 to list.size-1){
        val b_ijkl=Bernstein_ijkl(n,list.apply(i),barcenter)
        v(i)=b_ijkl

      }
      T(j)=VoxelsWithPixelvalues.apply(j)._2

      M(j,::):=v.t
    }

    val y=M\T

    BernsteinPolynomialInterpolation3D(n,y)
  }



}

case class BernsteinPolynomialInterpolation3D(degree:Int,coefficient:DenseVector[Double]) extends BernsteinPolynomialInterpolation[_3D] {
  val coefficients=coefficient

  val degreeofpolynomial=degree



  def ComputeDensityofPoint(u:DenseVector[Double]):Double={


    def coeficientIndexList (n:Int):IndexedSeq[(Int,Int,Int,Int)]={
      val seqvec= for {j<-0 to n; k<-0 to n; l<-0 to n; if j+k+l<=n} yield{

        (n-j-k-l,j,k,l)

      }
      seqvec

    }

    //this gives foctoriel of n
    def factorial(n: Int): Int = {

      if (n == 0) {
        1
      } else {
        var f = 1
        for (i <- 1 to n) {
          f = f * i
        }

        f
      }
    }
    def Bernstein_ijkl(n:Int,list:(Int,Int,Int,Int),u:DenseVector[Double]):Double ={
      (factorial(n)*Math.pow(u(0),list._1)*Math.pow(u(1),list._2)*Math.pow(u(2),list._3)*Math.pow(u(3),list._4))*
        (1.0/(factorial(list._1)*factorial(list._2)*factorial(list._3)*factorial(list._4)))
    }


    var sum=0.0
    for (c <- 0 to coeficientIndexList(degree).size-1){
      val list=coeficientIndexList(degree).apply(c)
      val v=Bernstein_ijkl(degree,list,u)
      sum=sum+coefficient(c)*v
    }
    sum

}






}