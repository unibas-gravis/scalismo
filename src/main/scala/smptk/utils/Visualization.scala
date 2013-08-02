package smptk
package utils

import image.ScalarPixel
import breeze.plot._
import breeze.linalg._
import vtk._
import scala.swing.SimpleSwingApplication
import scala.swing.Component
import javax.swing.JPanel
import java.awt.BorderLayout
import scala.swing.MainFrame
import smptk.image.DiscreteScalarImage1D
import smptk.mesh.TriangleMesh
import smptk.image.ContinuousScalarImage1D
import smptk.image.DiscreteImageDomain1D
import smptk.image.ContinuousVectorImage
import smptk.image.DiscreteScalarImage2D
import smptk.image.ContinuousScalarImage2D
import smptk.image.DiscreteImageDomain2D
import smptk.image.Resample
import smptk.image._
import smptk.common.BoxedDomain
import geometry._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import smptk.common.{ BoxedDomain, BoxedDomain1D, BoxedDomain2D, BoxedDomain3D }

object Visualization {

  private case class VTKMeshViewer(pd: vtkPolyData) extends SimpleSwingApplication {

    // Setup VTK rendering panel, this also loads VTK native libraries
    var renWin: vtkPanel = new vtkPanel

    // Create wrapper to integrate vtkPanel with Scala's Swing API
    val scalaPanel = new Component {
      override lazy val peer = new JPanel(new BorderLayout())
      peer.add(renWin)
    }

    var mapper = new vtkPolyDataMapper
    mapper.SetInputData(pd)

    var actor = new vtkActor
    actor.SetMapper(mapper)

    renWin.GetRenderer.AddActor(actor)
    renWin.GetRenderer.ResetCamera

    // Create the main application window
    override def top = new MainFrame {
      title = "ScaleCone"
      contents = scalaPanel
    }
  }

  private case class VTKImageViewer2D(sp: vtkStructuredPoints) extends SimpleSwingApplication {

    val vPanel = new vtkCanvas() {
      val style = new vtkInteractorStyleImage();
      style.SetInteractionModeToImage2D()
      val im = new vtkImageResliceMapper()
      val actor = new vtkImageActor()
      actor.SetInputData(sp)
      iren.SetInteractorStyle(style)
      ren.AddActor(actor)
      ren.SetRenderWindow(rw);
      iren.SetInteractorStyle(style);
      ren.ResetCamera()
      
      }

    val scalaPanel = new Component {
      override lazy val peer = new JPanel(new BorderLayout())
      peer.add(vPanel)
    }

    // Create the main application window
    override def top = new MainFrame {
      title = "ScaleImageViewer"
      contents = scalaPanel
    }
  }

  private case class VTKImageViewer3D(sp: vtkStructuredPoints) extends SimpleSwingApplication {

    val vPanel = new vtkCanvas() {
      val style = new vtkInteractorStyleImage();
      style.SetInteractionModeToImage3D()
      val im = new vtkImageResliceMapper()
      im.SetInputData(sp)
      im.SliceFacesCameraOn()
      im.SliceAtFocalPointOn()
      im.BorderOff()

      var actor = new vtkImageSlice
      actor.SetMapper(im)

      ren.AddActor(actor)
      ren.SetRenderWindow(rw);
      iren.SetInteractorStyle(style);
      ren.ResetCamera()

    }

    val scalaPanel = new Component {
      override lazy val peer = new JPanel(new BorderLayout())
      peer.add(vPanel)
    }

    // Create the main application window
    override def top = new MainFrame {
      title = "ScaleImageViewer"
      contents = scalaPanel
    }
  }

  //    case class ShowTwoVTK(pd1: vtkPolyData, pd2 : vtkPolyData) extends SimpleSwingApplication {
  //
  //    // Setup VTK rendering panel, this also loads VTK native libraries
  //    var renWin: vtkPanel = new vtkPanel
  //
  //    // Create wrapper to integrate vtkPanel with Scala's Swing API
  //    val scalaPanel = new Component {
  //      override lazy val peer = new JPanel(new BorderLayout())
  //      peer.add(renWin)
  //    }
  //
  //    var coneMapper = new vtkPolyDataMapper
  //    coneMapper.SetInputData(pd1)
  //
  //    var coneActor = new vtkActor
  //    coneActor.SetMapper(coneMapper)
  //    
  //    var coneMapper2 = new vtkPolyDataMapper
  //    coneMapper2.SetInputData(pd2)
  //
  //    var coneActor2 = new vtkActor
  //    coneActor2.SetMapper(coneMapper2)
  //    coneActor2.GetProperty().SetColor(1.0, 0.0, 0.0); //(R,G,B)
  //    
  //    renWin.GetRenderer.AddActor(coneActor)
  //    renWin.GetRenderer.AddActor(coneActor2)
  //    renWin.GetRenderer.ResetCamera
  //
  //    // Create the main application window
  //    override def top = new MainFrame {
  //      title = "ScaleCone"
  //      contents = scalaPanel
  //    }
  //  }

  def show(mesh: TriangleMesh) {
    val vtkpd = MeshConversion.meshToVTKPolyData(mesh)
    VTKMeshViewer(vtkpd).main(Array(""))
  }

  def show[D <: Dim, Pixel: ScalarPixel](img: DiscreteScalarImage1D[Pixel]): Unit = {
    val pixelConv = implicitly[ScalarPixel[Pixel]]
    val xs = img.domain.points.map(_(0).toDouble)

    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, img.pixelValues.map(pixelConv.toDouble(_)), '+')

  }

  def show[Pixel: ScalarPixel: ClassTag: TypeTag](img: DiscreteScalarImage2D[Pixel]) {
    val imgVTK = ImageConversion.image2DTovtkStructuredPoints(img)
    //    val imp = ImageConversion.image2DToImageJImagePlus(img)
    //    imp.show()
    VTKImageViewer2D(imgVTK).main(Array(""))
  }

  def show[Pixel: ScalarPixel: ClassTag: TypeTag](img: DiscreteScalarImage3D[Pixel]) {
    //    val imp = ImageConversion.image3DToImageJImagePlus(img)
    //    imp.show()
    val imgVTK = ImageConversion.image3DTovtkStructuredPoints(img)
    //    val imp = ImageConversion.image2DToImageJImagePlus(img)
    //    imp.show()
    VTKImageViewer3D(imgVTK).main(Array(""))

  }

  def show(img: ContinuousScalarImage1D, domain: BoxedDomain1D, outsideValue: Double): Unit = {

    val xs = linspace(domain.origin(0).toDouble, domain.extent(0), 512)
    val f = Figure()
    val p = f.subplot(0)

    p += plot(xs, xs.map(x => img.liftPixelValue(Point1D(x)).getOrElse(outsideValue).toDouble))
    f.refresh
    f.visible = true
  }

  def show(img: ContinuousScalarImage2D, domain: BoxedDomain2D, outsideValue: Double): Unit = {
    val spacing = (domain.extent - domain.origin) * (1.0 / 512.0)
    val discreteDomain = DiscreteImageDomain2D(domain.origin, spacing, Index2D(512, 512))
    val discreteImg = Resample.sample[Double](img, discreteDomain, outsideValue)
    show(discreteImg)
  }

  def show(img: ContinuousScalarImage3D, domain: BoxedDomain3D, outsideValue: Double): Unit = {
    val spacing = (domain.extent - domain.origin) * (1.0 / 256.0)
    val discreteDomain = DiscreteImageDomain3D(domain.origin, spacing, Index3D(256, 256, 256))
    val discreteImg = Resample.sample[Double](img, discreteDomain, outsideValue)
    show(discreteImg)
  }

  def main(args: Array[String]): Unit = {
    smptk.initialize()
    val lena = smptk.io.ImageIO.read3DScalarImage[Short](new java.io.File("/tmp/test.h5")).get
    //val lena = smptk.io.ImageIO.read2DScalarImage[Short](new java.io.File("/tmp/lena.h5")).get
    //val lena = smptk.io.MeshIO.readHDF5(new java.io.File("/tmp/facemesh.h5")).get
    show(lena)
  }
}

