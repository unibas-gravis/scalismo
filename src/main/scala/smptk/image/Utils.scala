package smptk
package image

import breeze.plot._
import breeze.linalg._
import Image._
import Geometry.CoordVector1D
import ij._
import ij.process.FloatProcessor
import ij.ImageStack
import ij.WindowManager
import smptk.registration.Transformation
import smptk.image.Geometry.{ Point1D, Point2D }
import smptk.image.Geometry.CoordVector2D
import smptk.common.BoxedRegion
import smptk.common.BoxedRegion2D
import smptk.numerics.Integrator
import smptk.common.BoxedRegion1D
import smptk.io.ImageIO
import smptk.io.MeshIO
import java.awt.BorderLayout
import javax.swing.JPanel
import swing._
import vtk._
import smptk.mesh.TriangleMesh
import smptk.mesh.TriangleMesh

object Utils {

  System.loadLibrary("vtkCommonJava");
  System.loadLibrary("vtkFilteringJava");
  System.loadLibrary("vtkIOJava");
  System.loadLibrary("vtkImagingJava");
  System.loadLibrary("vtkGraphicsJava");

  case class ShowVTK(pd: vtkPolyData) extends SimpleSwingApplication {

    // Setup VTK rendering panel, this also loads VTK native libraries
    var renWin: vtkPanel = new vtkPanel

    // Create wrapper to integrate vtkPanel with Scala's Swing API
    val scalaPanel = new Component {
      override lazy val peer = new JPanel(new BorderLayout())
      peer.add(renWin)
    }

    var coneMapper = new vtkPolyDataMapper
    coneMapper.SetInput(pd)

    var coneActor = new vtkActor
    coneActor.SetMapper(coneMapper)

    renWin.GetRenderer.AddActor(coneActor)
    renWin.GetRenderer.ResetCamera

    // Create the main application window
    override def top = new MainFrame {
      title = "ScaleCone"
      contents = scalaPanel
    }
  }

  def showVTK(pd: vtkPolyData) {
    ShowVTK(pd).main(Array(""))
  }

  def show1D[Pixel: ScalarPixel](img: DiscreteScalarImage1D[Pixel]) {
    val pixelConv = implicitly[ScalarPixel[Pixel]]

    val xs = img.domain.points.map(_(0).toDouble)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, img.pixelValues.map(pixelConv.toDouble(_)), '+')

  }

  def show1D(img: ContinuousScalarImage1D, domain: DiscreteImageDomain1D, outsideValue: Double = 0) {

    val xs = linspace(domain.origin(0).toDouble, domain.extent(0), domain.numberOfPoints)
    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img.liftPixelValue(CoordVector1D(x)).getOrElse(outsideValue).toDouble))
    f.refresh
    f.visible = true
  }

  def show1D(img: ContinuousVectorImage[CoordVector1D], domain: DiscreteImageDomain1D) {

    val xs = linspace(domain.origin(0).toDouble, domain.extent(0), domain.numberOfPoints)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img(CoordVector1D(x))(0)))

  }

  def showGrid1D(domain: DiscreteImageDomain1D, transform: Transformation[CoordVector1D]) {
    val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => if (math.round(x(0)) % 2 == 0) -1. else 1.).toIndexedSeq )
    //val discreteImage = DiscreteScalarImage1D(domain, domain.points.map(x => x(0)))

    val continuousImg = Interpolation.interpolate1D(3)(discreteImage)
    show1D(continuousImg compose transform, domain)
  }

  def show2D[Pixel: ScalarPixel](img: DiscreteScalarImage2D[Pixel]) {
    val pixelConv = implicitly[ScalarPixel[Pixel]]
    val domain = img.domain

    val bp = new FloatProcessor(domain.size(0), domain.size(1), img.pixelValues.map(pixelConv.toFloat(_)).toArray)
    val imp = new ImagePlus("2D image", bp)
    imp.show()
  }

  def show2D(img: ContinuousScalarImage2D, domain: DiscreteImageDomain2D, outsideValue: Float = 0) {
    val discreteImg = Resample.sample2D(img, domain, outsideValue)
    show2D(discreteImg)
  }

  
  def show3D[Pixel: ScalarPixel](img: DiscreteScalarImage3D[Pixel]) {
 
    val imPlus = imageToImageJImagePlus(img)
    imPlus.show()
  }

    def show3D(img: ContinuousScalarImage3D, domain: DiscreteImageDomain3D, outsideValue: Float = 0) {
    val discreteImg = Resample.sample3D(img, domain, outsideValue)
    show3D(discreteImg)
  }

  
  def gridImage2D(gridWidth: Double, tolerance: Double): ContinuousScalarImage2D = {
    def grid(x: Point2D) = {
      if (math.abs(x(0) % gridWidth) < tolerance || math.abs(x(1) % gridWidth) < tolerance) 0f else 1f
    }
    def df(x: Point2D) = DenseVector(0., 0.)
    ContinuousScalarImage2D((x: Point2D) => true, grid, Some(df))

  }

  def gridImage1D(gridWidth: Double, tolerance: Double): ContinuousScalarImage1D = {
    def grid(x: Point1D) = {
      if (math.abs(x(0) % gridWidth) < tolerance) 0f else 1f
    }
    def df(x: Point1D) = DenseVector(0.)
    ContinuousScalarImage1D((x: Point1D) => true, grid, Some(df))
  }

  def gaussianSmoothing1D(img: ContinuousScalarImage1D, deviation: Double, integrator: Integrator[CoordVector1D]) = {
    img.convolve(GaussianFilter1D(deviation), integrator)
  }

  def gaussianSmoothing2D(img: ContinuousScalarImage2D, deviation: Double, integrator: Integrator[CoordVector2D]) = {
    img.convolve(GaussianFilter2D(deviation), integrator)
  }

//  def gaussianSmoothing3D(img: ContinuousScalarImage3D, deviation: Double, integrator: Integrator[CoordVector3D]) = {
//    img.convolve(GaussianFilter3D(deviation), integrator)
//  }
  
  def imageToImageJImagePlus[Pixel : ScalarPixel](img : DiscreteScalarImage3D[Pixel]) = { 
       val pixelConv = implicitly[ScalarPixel[Pixel]]
    val domain = img.domain
    val (width, height, size) = (domain.size(0), domain.size(1), domain.size(2))

    // 	Create 3x3x3 3D stack and fill it with garbage  
    val stack = new ImageStack(width, height)

    val pixelValues = img.pixelValues.map(pixelConv.toFloat(_))
    for (slice <- 0 until size) {
      val startInd = slice * (width * height)
      val endInd = (slice + 1) * (width * height)
      val pixelForSlice = pixelValues.slice(startInd, endInd).toArray
      val bp = new FloatProcessor(width, height, pixelForSlice)
      stack.addSlice(bp)

    }
    new ImagePlus("3D image", stack)
  }
  
  def meshToVTKMesh(mesh: TriangleMesh): vtkPolyData = {
    val pd = new vtkPolyData()
    val domain = mesh.domain
    val pts = new vtkPoints()
    pts.SetNumberOfPoints(domain.points.size)
    for ((pt, pt_id) <- domain.points.zipWithIndex) { 
    	pts.InsertPoint(pt_id, pt(0), pt(1), pt(2))
    }
    pd.SetPoints(pts)
    
    val triangles = new vtkCellArray
    triangles.SetNumberOfCells(domain.cells.size)
    for ((cell, cell_id) <- domain.cells.zipWithIndex) { 
    	val triangle = new vtkTriangle()

    	triangle.GetPointIds().SetId ( 0, cell.ptId1 );
    	triangle.GetPointIds().SetId ( 1, cell.ptId2);
    	triangle.GetPointIds().SetId ( 2, cell.ptId3 );
    	triangles.InsertNextCell(triangle );
    }
    pd.SetPolys(triangles)
    pd
  }


  def main(args: Array[String]) {
    import java.io.File
    val mesh = MeshIO.readHDF5(new File("/tmp/mesh.h5")).get
    val vtkpd = meshToVTKMesh(mesh)
    showVTK(vtkpd)
  }

}