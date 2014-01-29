package org.statismo.stk.core.filters

import org.statismo.stk.core.image.DiscreteImage2D
import org.statismo.stk.core.image.DiscreteScalarImage2D
import org.statismo.stk.core.image.DiscreteScalarImage1D
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import org.statismo.stk.core.image.DiscreteScalarImage3D
import vtk.vtkImageEuclideanDistance
import reflect.runtime.universe.{ TypeTag, typeOf }
import org.statismo.stk.core.utils.ImageConversion
import scala.reflect.ClassTag
import scala.util.Success
import scala.util.Failure
import vtk.vtkImageCast
import org.statismo.stk.core.common.ScalarValue


object DistanceTransform {

  /**
   * Computes the distance transform of a (discrete)  binary image
   */
  def euclideanDistanceTransform[Scalar: ScalarValue: ClassTag: TypeTag](img: DiscreteScalarImage3D[Scalar]): DiscreteScalarImage3D[Float] = {
    val scalarConv = implicitly[ScalarValue[Scalar]]

    def doDistanceTransformVTK(img : DiscreteScalarImage3D[Scalar]) = {
      val imgvtk = ImageConversion.image3DTovtkStructuredPoints(img)
      val vtkdistTransform = new vtkImageEuclideanDistance()
      vtkdistTransform.ConsiderAnisotropyOn()
      vtkdistTransform.SetInputData(imgvtk)
      vtkdistTransform.SetMaximumDistance(100000)
      vtkdistTransform.InitializeOn()
      vtkdistTransform.ReleaseDataFlagOn()

      // vtk gives double precision - we rather want to have float
      val caster = new vtkImageCast()
      caster.SetOutputScalarTypeToFloat()
      caster.SetInputConnection(vtkdistTransform.GetOutputPort())
      caster.Update()
      val dtvtk = caster.GetOutput()
      val dt = ImageConversion.vtkStructuredPointsTo3DScalarImage[Float](dtvtk) match {
        case Success(dt) => dt.map(v => math.sqrt(v).toFloat)
        case Failure(ex) => throw new Exception(ex)
      }
      caster.Delete()
      imgvtk.Delete()
      dtvtk.Delete()
      vtkdistTransform.Delete()     
      System.gc() // make sure it deletes the intermediate resuls
      dt
    }

    // vtk only computes the distance to either the 0 or the 1 part. we thus create two distnace images and combine them 
    val dt1 = doDistanceTransformVTK(img)
    val invImg = img.map[Scalar](v => if (v == 0) scalarConv.fromShort(1) else scalarConv.fromShort(0))
    val dt2 = doDistanceTransformVTK(invImg)
    
    val newPixelValues = dt1.values.view.zip(dt2.values.view).map{case (p1, p2) => p1 + p2}.toArray
    DiscreteScalarImage3D(dt1.domain, newPixelValues)
  }

  /**
   * Computes the distance transform of a (discrete)  binary image
   */
  def euclideanDistanceTransform2D[Scalar: ScalarValue : ClassTag](img: DiscreteScalarImage2D[Scalar]): DiscreteScalarImage2D[Scalar] = {
    val scalarConv = implicitly[ScalarValue[Scalar]]

    val zeroInftyPixelValues = for (pv <- img.values) yield if (scalarConv.toDouble(pv) == 0.0) 0.0 else Double.MaxValue

    val squaredDistanceValues = FelzenszwalbHuttenlocherDT2(zeroInftyPixelValues, (img.domain.size(0), img.domain.size(1)))
    val newPixelValues = squaredDistanceValues.map(f => scalarConv.fromDouble(math.sqrt(f))).toArray
    DiscreteScalarImage2D(img.domain, newPixelValues)
  }

  def euclideanDistanceTransform1D[Scalar: ScalarValue : ClassTag](img: DiscreteScalarImage1D[Scalar]): DiscreteScalarImage1D[Scalar] = {

    val scalarConv = implicitly[ScalarValue[Scalar]]
    val zeroInftyPixelValues = for (pv <- img.values) yield if (scalarConv.toDouble(pv) == 0.0) 0.0 else Double.MaxValue
    val squaredDistanceValues = FelzenszwalbHuttenlocherDT1(zeroInftyPixelValues)

    val newPixelValues = squaredDistanceValues.map(f => scalarConv.fromDouble(math.sqrt(f)))
    DiscreteScalarImage1D(img.domain, newPixelValues)
  }

  private def FelzenszwalbHuttenlocherDT2(f: Array[Double], size: (Int, Int)): Array[Double] = {
    val (dimX, dimY) = size
    val newPixelMat = DenseMatrix.create[Double](dimX, dimY, f.toArray).t

    // transform rows 
    for (i <- (0 until newPixelMat.rows).par) {
      val row = newPixelMat(i, ::)
      val trow = FelzenszwalbHuttenlocherDT1(row.toDenseVector.toArray)
      newPixelMat(i, ::) := DenseVector(trow.toArray)
    }

    // transform cols 
    for (j <- (0 until newPixelMat.cols).par) {
      val col = newPixelMat(::, j)
      val tcol = FelzenszwalbHuttenlocherDT1(col.toDenseVector.toArray)
      newPixelMat(::, j) := DenseVector(tcol.toArray)
    }

    newPixelMat.data
  }

  private def FelzenszwalbHuttenlocherDT1(f: Array[Double]): Array[Double] = {

    val n = f.size
    var k = 0
    val v = new Array[Int](n) // location of parabolas
    val z = new Array[Double](n + 1) // location of boundaries between parabolas
    val Df = new Array[Double](n)
    z(0) = -Double.MaxValue
    z(1) = Double.MaxValue
    v(0) = 0

    for (q <- 1 until n) {

      var s = ((f(q) + math.pow(q, 2)) - (f(v(k)) + math.pow(v(k), 2))) / (2 * q - 2 * v(k))
      while (s <= z(k)) {
        k -= 1
        s = ((f(q) + math.pow(q, 2)) - (f(v(k)) + math.pow(v(k), 2))) / (2 * q - 2 * v(k))
      }

      k += 1
      v(k) = q
      z(k) = s
      z(k + 1) = Double.MaxValue
    }
    k = 0
    for (q <- 0 until n) {
      while (z(k + 1) < q) {
        k += 1
      }
      Df(q) = (q - v(k)) * (q - v(k)) + f(v(k))
    }

    Df
  }

}