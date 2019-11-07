package scalismo.utils

import java.io.File

import breeze.linalg.{ DenseMatrix, DenseVector }
import scalismo.common._
import scalismo.geometry._
import scalismo.image.DiscreteScalarImage
import scalismo.io.{ ImageIO, LandmarkIO, TetraMeshIO }
import scalismo.kernels.{ DiagonalKernel, GaussianKernel, MatrixValuedPDKernel, PDKernel }
import scalismo.numerics.RandomMeshVolumeSampler3D
import scalismo.statisticalmodel.dataset.{ DataCollection, DataCollectionOfMeshVolume }
import scalismo.statisticalmodel._
import scalismo.tetramesh.{ TetrahedralCell, TetrahedralMesh }
import vtk.vtkTetra
import scalismo.utils.Random.implicits._
object main {
  def main(args: Array[String]): Unit = {
    scalismo.initialize()

    val tetramesh = TetraMeshIO.readTetraMesh(new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\tetraMesh.vtk")).get


    TetraMeshIO.writeTetraMesh(tetramesh,new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\tetraMesh1.vtk")).get
   // val image = ImageIO.read3DScalarImage[Short](new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\VSD.Body.040Y.M.CT.57768i.nii")).get
    //val tetra = TetraMeshIO.readTetraMesh(new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\humerusvolume.inp")).get
    /*val land = LandmarkIO.readLandmarksJson[_3D](new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\landmarksfromimagetodefinetetra.json")).get






    val pts = IndexedSeq(land.toIndexedSeq.apply(0).point, land.toIndexedSeq.apply(1).point, land.toIndexedSeq.apply(2).point, land.toIndexedSeq.apply(3).point)
    val cells = IndexedSeq(TetrahedralCell(PointId(0), PointId(1), PointId(2), PointId(3)))
    val tetramesh = TetrahedralMesh3D(UnstructuredPointsDomain(pts), TetrahedralList(cells))

    val tetra = TetraMeshIO.writeTetraMesh(tetramesh,new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\Imatetramesh.vtu")).get
*/

    /* val tpolycoef=GetTetrahedralMeshDenssityFunctionFromImage(tetramesh,image,4)


   val tv=tetramesh.tetrahedrons.map(t=>(Seq(tetramesh.pointSet.point(t.ptId1),tetramesh.pointSet.point(t.ptId2),tetramesh.pointSet.point(t.ptId3),tetramesh.pointSet.point(t.ptId4)),
     GetTetrahedronVoxels(tetramesh,t,image)))

   // val land=GetTetrahedronVoxels(tetramesh,tetramesh.tetrahedrons.apply(0),image).map(p=>Landmark[_3D]("i",p._1))

    // LandmarkIO.writeLandmarksJson[_3D](land,(new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\tetrahedronpoints.json")))



   val lis=tv zip tpolycoef
   // val t=lis.map(p=>p._1._2.map(e=>((p._1._1,PointToDensityValue(e._1,p._1._1.toIndexedSeq,p._2,4)-e._2.toDouble))))
  //  val t=lis.map(p=>p._1._2.map(e=>(image.domain.findClosestPoint(e._1).id,e._1,PointToDensityValue(e._1,p._1._1.toIndexedSeq,p._2,30).toShort)))
   //println(t.apply(0))


println("finish result computation, start visualation computation")

   // val dom=UnstructuredPointsDomain[_3D](GetTetrahedronVoxels(tetramesh,tetramesh.tetrahedrons.apply(0),image).map(p=>p._1).toIndexedSeq)
    //val value=image.domain.points.toSeq.map(p=> if( dom.isDefinedAt(p)) PointToDensityValue(p,lis.apply(0)._1._1.toIndexedSeq,lis.apply(0)._2,8).toShort else 0.0.toShort)
    val value=image.domain.points.toSeq.map { p =>
      val s = imagepointTotetrahedron(p, tetramesh, image, lis)
      if (s._1 == 1) {
        PointToDensityValue(p, s._2, s._3, 4).toShort
      } else {
        0.0.toShort
      }
    }

    val sampleImage : DiscreteScalarImage[_3D, Short] = DiscreteScalarImage[_3D,Short](image.domain,value)
    ImageIO.writeNifti(sampleImage, new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\sampleImages.nii"))

  //  val valueo=image.pointsWithValues.map(p=> if( dom.isDefinedAt(p._1)) p._2 else 0.0.toShort).toSeq
  val valueo=image.pointsWithValues.map(p=> if(imagepointTotetrahedron(p._1,tetramesh,image,lis)._1==1) p._2 else 0.0.toShort).toSeq

    val sampleImageo : DiscreteScalarImage[_3D, Short] = DiscreteScalarImage[_3D,Short](image.domain,valueo)
    ImageIO.writeNifti(sampleImageo, new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\sampleImageso.nii"))

    val vals=(sampleImage.pointsWithValues zip sampleImageo.pointsWithValues).map(p=>(p._1._2-p._2._2).toShort).toSeq

    val sampleImage2: DiscreteScalarImage[_3D, Short] = DiscreteScalarImage[_3D,Short](image.domain,vals)
    ImageIO.writeNifti(sampleImage2, new File("E:\\PhD folders\\Tetrahedral mesh\\test file\\sampleImageserr.nii"))
*/
/*
   val master_gaussKernel: PDKernel[_3D] = GaussianKernel[_3D](200) * 10

    val model: StatisticalMeshVolumeModel = createBasicFFModel(tetramesh, master_gaussKernel, 40, 10)

    println(model.referenceMeshVolume.volume)
    println(model.rank)

    val dc = DataCollectionOfMeshVolume.fromMeshDirectory(tetramesh, new File("E:\\PhD folders\\data for PGA model 21-03-2019\\New registered shoulders data\\scapula for SSM\\"))._1.get
*/
    //val ssm=SSM(dc).gp.posterior()

    /*val v=DenseVector.zeros[Double](4).toVector



    def transformForPoint(refPoint: Point[_3D]): DenseVector[Double] = {
      val id = tetramesh.pointSet.findClosestPoint(refPoint).id
      val pt =tetramesh.pointSet.point(id)
      DenseVector((pt - refPoint)(0),(pt - refPoint)(1),(pt - refPoint)(2),1.0,0.0)
    }

    val sq=Seq(Field(RealSpace[_3D], transformForPoint _))



    implicit val scalarVectorizer = new Vectorizer[DenseVector[Double]] {
      override def dim = 5

      override def vectorize(v: DenseVector[Double]) = v

      override def unvectorize(d: DenseVector[Double]) = d
    }

     val m=DiscreteLowRankGaussianProcess.createUsingPCA(tetramesh.pointSet, sq)

*/

   /* val domain12D = UnstructuredPointsDomain(tetramesh.tetrahedralization.tetrahedrons.map(t =>
      Point12D(tetramesh.pointSet.point(t.ptId1).x, tetramesh.pointSet.point(t.ptId1).y, tetramesh.pointSet.point(t.ptId1).z,
        tetramesh.pointSet.point(t.ptId2).x, tetramesh.pointSet.point(t.ptId2).y, tetramesh.pointSet.point(t.ptId2).z,
        tetramesh.pointSet.point(t.ptId3).x, tetramesh.pointSet.point(t.ptId3).y, tetramesh.pointSet.point(t.ptId3).z,
        tetramesh.pointSet.point(t.ptId4).x, tetramesh.pointSet.point(t.ptId4).y, tetramesh.pointSet.point(t.ptId4).z)))

    def transformForPoint(refPoint: Point[_12D]) = {
      //val id = tetramesh.pointSet.findClosestPoint(refPoint).id
      val pt = Point12D(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
      DenseVector((pt - refPoint)(0), (pt - refPoint)(1), (pt - refPoint)(2), 1.0, 0.0)
    }

    val sq1 = Seq(Field(RealSpace[_12D], transformForPoint _))

    implicit val scalarVectorizer = new Vectorizer[DenseVector[Double]] {
      override def dim = 15 //size return value of the return  deformation field

      override def vectorize(v: DenseVector[Double]) = v

      override def unvectorize(d: DenseVector[Double]) = d
    }

    val m = DiscreteLowRankGaussianProcess.createUsingPCA(domain12D, sq1)
*/
    //var r=0

    // val d=tv.map(e=>e._2.map(p=>{r=r+1;(p._1,PointToDensityValue(p._1,e._1.toIndexedSeq,tpolycoef.apply(r),3))}))

    //val pts = IndexedSeq(Point(0.0, 0.0, 0.0), Point(1.0, 1.0, 1.0), Point(1.0, 1.0, 5.0), Point(1.0, 1.0, -5.0))
    // val v=DenseVector[Double](0.0,1.0,2.0,3.0)
    //  PointToDensityValue(Point3D(0.0,0.0,0.0),pts,v,3)

    /* val M=DenseMatrix((-11.0,2.0),(2.0,3.0),(2.0,-1.0))
    val T=DenseVector(0.0,7.0,5.0)

    //val SVD(u, d2, vt) = breeze.linalg.svd(M.t*M)
    // val (r1,r2)= breeze.linalg.ReducedSVD(M)
    val SVD(u, s, vt) = breeze.linalg.svd.reduced(M)
    //println(M.rows,M.cols)
    println(u)
    println()
    println(vt)

    val Dinv = s.map(d => if (d > 1e-6) 1.0 / d else 0.0)
    val D=breeze.linalg.diag(Dinv)
   // println(vt.t*D*u.t)

    println(vt*D*u.t*T)
*/

  }

  def SSM(dc: DataCollectionOfMeshVolume): StatisticalMeshVolumeModel = {
    import scalismo.utils.Random.implicits._
    val gpaAligned: DataCollectionOfMeshVolume = DataCollectionOfMeshVolume.gpa(dc)
    val modelAligned = StatisticalMeshVolumeModel.createUsingPCA(gpaAligned).get

    return modelAligned
  }

  def createBasicFFModel(refModel: TetrahedralMesh[_3D], gKernel: PDKernel[_3D], numSamples: Int, numBasisFunctions: Int): StatisticalMeshVolumeModel = {
    val zeroMean = VectorField(RealSpace[_3D], (pt: Point[_3D]) => EuclideanVector3D(0, 0, 0))
    val land = LandmarkIO.readLandmarksJson[_3D](new File("E:\\PhD folders\\data for PGA model 21-03-2019\\test for tedy registration\\testbiaskernel.json")).get
    val matrixValuedGaussian1: MatrixValuedPDKernel[_3D] = DiagonalKernel[_3D](gKernel, 3)

    //val matrixValuedGaussian2=DiagonalKernel[_3D](BiasGuassainKernel(900.0,land.head.point)*10,3)
    // val matrixValuedGaussian=matrixValuedGaussian1+matrixValuedGaussian2
    val gp: GaussianProcess[_3D, EuclideanVector[_3D]] = GaussianProcess(zeroMean, matrixValuedGaussian1)
    import scalismo.utils.Random.implicits._
    val sampler = RandomMeshVolumeSampler3D(refModel, numSamples, 42) //500
    val lowRankGP = LowRankGaussianProcess.approximateGP(gp, sampler, numBasisFunctions) //50
    //val defField = lowRankGP.sampleAtPoints(femur)
    //val discreteLowRankGP: DiscreteLowRankGaussianProcess[_3D,_3D] = lowRankGP.discretize(femurRef)
    StatisticalMeshVolumeModel(refModel, lowRankGP)
  }

  /**
   *  this function wether an image point belongs to one of the tetrahdron and returns
   *  a triple where  Int: (1 if it belongs 0 otherwise ); seq(Point[_3D]): tetrahedron; DenseVetor: denisity function coeficients
   *
   *
   */

  def imagepointTotetrahedron(p: Point[_3D], tetramesh: TetrahedralMesh[_3D], image: DiscreteScalarImage[_3D, Short],
    l: IndexedSeq[((Seq[Point[_3D]], scala.IndexedSeq[(Point[_3D], Short)]), DenseVector[Double])]): (Int, IndexedSeq[Point[_3D]], DenseVector[Double]) = {

    var bol = 0
    var i = 0
    while ((bol == 0) && (i < tetramesh.tetrahedrons.size)) {
      //val dom=UnstructuredPointsDomain[_3D](GetTetrahedronVoxels(tetramesh,tetramesh.tetrahedrons.apply(i),image).map(p=>p._1).toIndexedSeq)
      if (tetramesh.isInsideTetrahedralCell(p, tetramesh.tetrahedrons.apply(0))) {
        bol = 1
      } else {
        i = i + 1
      }

    }

    if (bol == 1) {
      (1, l.apply(i)._1._1.toIndexedSeq, l.apply(i)._2)

    } else {
      (0, l.apply(i - 1)._1._1.toIndexedSeq, l.apply(i - 1)._2)
    }

  }

  /**
   *  this function eturns the a sequence of volex with their corresponding pixel value  of a given tetrahedrons
   *
   *
   */

  def GetTetrahedronVoxels(tetra: TetrahedralMesh[_3D], t: TetrahedralCell, image: DiscreteScalarImage[_3D, Short]): IndexedSeq[(Point[_3D], Short)] = {
    val l = image.pointsWithValues.flatMap { pid => if (tetra.isInsideTetrahedralCell(pid._1, t)) List(pid) else List() }.toIndexedSeq

    // println("number of voxel is "+l.size)
    l
  }

  /**
   *  this function returns the a sequence berstein polynomial coeficient of tetrahedrons (defining the tetrahedral mesh)
   * the densevector at the index i contains the poly coef of the tetrahedron (cell) at the index i.
   *
   *
   */
  def GetTetrahedralMeshDenssityFunctionFromImage(tetra: TetrahedralMesh[_3D], image: DiscreteScalarImage[_3D, Short], degree: Int): IndexedSeq[DenseVector[Double]] = {

    val s = for (i <- 0 to tetra.tetrahedrons.size - 1) yield {
      val t = tetra.tetrahedrons.apply(i)
      (GetTetrahedronVoxels(tetra, t, image), Seq(tetra.pointSet.point(t.ptId1), tetra.pointSet.point(t.ptId2), tetra.pointSet.point(t.ptId3), tetra.pointSet.point(t.ptId4)).toIndexedSeq)
    }
    // println(s.apply(0)._1.map(p=>p._2))
    s.map(sq => density_fuction(sq._1, sq._2, degree))

  }

  /**
   *  this function returns baricentric coordinate of a point inside a tetrahedron
   *
   *
   *
   */
  def baryOfPointIntetrahedron(p: Point[_3D], a: Point[_3D], b: Point[_3D], c: Point[_3D], d: Point[_3D]): DenseVector[Double] = {

    val bcoord = new Array[Double](4)
    //this to initialised the array where the result will be stored
    val tetrahedron = new vtkTetra()
    tetrahedron.BarycentricCoords(p.toArray, a.toArray, b.toArray, c.toArray, d.toArray, bcoord)
    DenseVector[Double](bcoord.apply(0), bcoord.apply(1), bcoord.apply(2), bcoord.apply(3))
  }

  def PointToDensityValue(p: Point[_3D], points: IndexedSeq[Point[_3D]], coef: DenseVector[Double], deg: Int): Double = {

    val baric = baryOfPointIntetrahedron(p, points.apply(0), points.apply(1), points.apply(2), points.apply(3))
    val permutlist = PermutList(deg)

    var sum = 0.0
    //val size=3//permutlist.size-1

    for (c <- 0 to permutlist.size - 1) {
      val list = permutlist.apply(c)
      val v = Bernstein_ijkl(deg, list(0).toInt, list(1).toInt, list(2).toInt, list(3).toInt, baric)
      sum = sum + coef(c) * v
    }
    sum
  }

  /**
   * this function give the value of the baricentric Bernstein basis for a given baricentric coordinates
   * it works by calulating B^{n}_{i,j,k,l}(u=(u_x,u_y,u_z,u_w))=n!/(i!j!k!l!)u^i_xu^j_yu^k_zu^l_w
   *
   * u=(u_x,u_y,u_z,u_w) barentric coordinates
   * n degree of the Bernstein polynomial
   *
   *
   *
   */
  def Bernstein_ijkl(n: Int, i: Int, j: Int, k: Int, l: Int, u: DenseVector[Double]): Double = {
    (factorial(n) * Math.pow(u(0), i) * Math.pow(u(1), j) * Math.pow(u(2), k) * Math.pow(u(3), l)) * (1.0 / (factorial(i) * factorial(j) * factorial(k) * factorial(l)))
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

  /**
   * this function compute the coeficient of the desnsity function of a tetrahehron
   *
   * tetrahedron: Seq[Point[_3D],  sequence of point defining the tetrahedron
   * seqvoxelPixel: Seq[(Point[_3D],Double)], sequence of voxel inside the tetrahedron
   * and their corresponding pivel values
   * n: degree of berstein polynome
   */
  def PermutList(n: Int): Seq[DenseVector[Double]] = {

    val seqvec = for { j <- 0 to n; k <- 0 to n; l <- 0 to n; if j + k + l <= n } yield {

      DenseVector[Double](n - j - k - l, j, k, l)

    }
    seqvec.toSeq

  }

  /**
   * this function compute the coeficient of the desnsity function of a tetrahehron
   * tetrahedron: Seq[Point[_3D],  sequence of point defining the tetrahedron
   * seqvoxelPixel: Seq[(Point[_3D],Double)], sequence of voxel inside the tetrahedron
   * and their corresponding pivel values
   * n: degree of berstein polynome
   *
   *
   */
  def density_fuction(seqvoxelPixel: Seq[(Point[_3D], Short)], tetrahedron: Seq[Point[_3D]], degpol: Int): DenseVector[Double] = {

    val list = PermutList(degpol)
    println("number of pixel for matrix" + seqvoxelPixel.size)

    var M = DenseMatrix.zeros[Double](seqvoxelPixel.size, list.size) //the -1 here is to remove the sezo vector that was added to list
    var T = DenseVector.zeros[Double](seqvoxelPixel.size)

    for (j <- 0 to seqvoxelPixel.size - 1) {
      var v = DenseVector.zeros[Double](list.size)

      val barcenter = baryOfPointIntetrahedron(seqvoxelPixel.apply(j)._1, tetrahedron.apply(0), tetrahedron.apply(1), tetrahedron.apply(2), tetrahedron.apply(3))
      for (i <- 0 to list.size - 1) {
        val b_ijkl = Bernstein_ijkl(degpol, list.apply(i)(0).toInt, list.apply(i)(1).toInt, list.apply(i)(2).toInt,
          list.apply(i)(3).toInt, barcenter)
        v(i) = b_ijkl

      }
      T(j) = seqvoxelPixel.apply(j)._2

      M(j, ::) := v.t
    }

    // val QR(q,r)=breeze.linalg.qr.reduced(M)

    // val SVD(u, s, vt) = breeze.linalg.svd.reduced(M)

    // val Dinv = s.map(d => if (d > 1e-6) 1.0 / d else 0.0)
    // val D=breeze.linalg.diag(Dinv)

    // val x=(vt.t*D*u.t)*T//vt*D*u.t*T

    //  val sol=M*(vt.t*D*u.t)*T-T

    val y = M \ T

    y
  }

}
