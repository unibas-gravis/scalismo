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
import smptk.statisticalmodel.StatisticalMeshModel
import scala.swing.Slider
import scala.swing.BoxPanel
import scala.swing.Orientation
import smptk.io.StatismoIO
import scala.swing.event.ValueChanged
import java.awt.geom.Dimension2D
import java.awt.Dimension
import scala.swing.Button
import scala.swing.event.ButtonClicked
import smptk.io.MeshIO
import javax.swing.UIManager
import javax.swing.WindowConstants.{ DISPOSE_ON_CLOSE, DO_NOTHING_ON_CLOSE }

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
      peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
      override def closeOperation() { dispose() }
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
      peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
      override def closeOperation() { dispose() }
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
      peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
      override def closeOperation() { dispose() }

    }
  }

  private case class VTKStatmodelViewer(statmodel: StatisticalMeshModel) extends SimpleSwingApplication {
    val mesh = statmodel.mesh
    val pd = MeshConversion.meshToVTKPolyData(mesh)
    val gp = statmodel.gp.specializeForPoints(mesh.points.force)
    val numComps = gp.eigenPairs.size
    val coeffs = DenseVector.zeros[Float](numComps)

    // Setup VTK rendering panel, this also loads VTK native libraries
    var renWin: vtkPanel = new vtkPanel
    val sliders = for (i <- 0 until 30) yield {
      val slider = new Slider()
      slider.min = -5
      slider.max = 5
      slider.snapToTicks = true
      slider.majorTickSpacing = 1
      slider.name = i.toString
      slider.value = 0
      slider
    }
    val resetButton = new Button()
    resetButton.text = "reset"
    resetButton.name = "reset"

    val randomButton = new Button()
    randomButton.text = "random"
    randomButton.name = "random"
    val sliderPanel = new BoxPanel(Orientation.Vertical) {
      for (slider <- sliders) {
        contents += slider
        contents += resetButton
        contents += randomButton
      }

    }
    // Create wrapper to integrate vtkPanel with Scala's Swing API
    val scalaPanel = new Component {
      override lazy val peer = new JPanel(new BorderLayout())
      peer.add(renWin)
    }

    var mapper = new vtkPolyDataMapper
    mapper.SetInputData(pd)

    var actor = new vtkActor
    actor.SetMapper(mapper)

    updateMesh(coeffs) // render the mean
    renWin.GetRenderer.AddActor(actor)
    renWin.GetRenderer.ResetCamera

    def updateMesh(coeffs: DenseVector[Float]) {
      val ptdefs = gp.instanceAtPoints(coeffs)
      val newptseq = for ((pt, df) <- ptdefs) yield (Array((pt(0) + df(0)).toFloat, (pt(1) + df(1)).toFloat, (pt(2) + df(2)).toFloat))
      val ptarray = newptseq.flatten
      val arrayVTK = new vtkFloatArray()
      arrayVTK.SetNumberOfValues(mesh.numberOfPoints)
      arrayVTK.SetNumberOfComponents(3)
      arrayVTK.SetJavaArray(ptarray.toArray)

      pd.GetPoints().SetData(arrayVTK)
      pd.Modified()
      renWin.Render()

    }

    // Create the main application window
    override def top = new MainFrame {
      title = "ScaleCone"

      peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
      override def closeOperation() { dispose() }

      contents = new BoxPanel(Orientation.Horizontal) {
        contents += scalaPanel
        contents += sliderPanel
      }
      size = new Dimension(1024, 700)
      for (slider <- sliders) {
        listenTo(slider)
      }
      listenTo(resetButton)
      listenTo(randomButton)
      reactions += {
        case ValueChanged(s) => {
          val slider = s.asInstanceOf[Slider]
          coeffs(slider.name.toInt) = slider.value.toFloat
          updateMesh(coeffs)
        }
        case ButtonClicked(b) => {
          b.name match {
            case "reset" => {
              for (slider <- sliders) slider.value = 0
              coeffs.foreach(s => 0)
              updateMesh(coeffs)
            }
            case "random" => {
              val gaussian = breeze.stats.distributions.Gaussian(0, 1)
              coeffs.foreach(s => 0)
              for ((slider, i) <- sliders.zipWithIndex) {
                val r = gaussian.draw.toFloat
                slider.value = 0
                coeffs(i) = r
              }
              updateMesh(coeffs)
            }
          }

        }

      }

    }
  }

  def show(mesh: TriangleMesh) {
    val vtkpd = MeshConversion.meshToVTKPolyData(mesh)
    VTKMeshViewer(vtkpd).main(Array(""))
    vtkpd.Delete()
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
    VTKImageViewer2D(imgVTK).main(Array(""))
    imgVTK.Delete()
  }

  def show[Pixel: ScalarPixel: ClassTag: TypeTag](img: DiscreteScalarImage3D[Pixel]) {
    val imgVTK = ImageConversion.image3DTovtkStructuredPoints(img)
    VTKImageViewer3D(imgVTK).main(Array(""))
    imgVTK.Delete()

  }

  def show(img: ContinuousScalarImage1D, domain: BoxedDomain1D, outsideValue: Double): Unit = {

    val xs = linspace(domain.origin(0), domain.extent(0), 512)
    val f = Figure()
    val p = f.subplot(0)

    val liftedImg = (x : Double) => img.liftPixelValue(Point1D(x.toFloat)).getOrElse(outsideValue.toFloat).toDouble
    p += plot(xs, xs.map(x => liftedImg(x)))
    f.refresh
    f.visible = true
  }

  def show(img: ContinuousScalarImage2D, domain: BoxedDomain2D, outsideValue: Double): Unit = {
    val spacing = (domain.extent - domain.origin) * (1.0 / 512.0).toFloat
    val discreteDomain = DiscreteImageDomain2D(domain.origin, spacing, Index2D(512, 512))
    val discreteImg = Resample.sample[Double](img, discreteDomain, outsideValue)
    show(discreteImg)
  }

  def show(img: ContinuousScalarImage3D, domain: BoxedDomain3D, outsideValue: Double): Unit = {
    val spacing = (domain.extent - domain.origin) * (1.0 / 256.0).toFloat
    val discreteDomain = DiscreteImageDomain3D(domain.origin, spacing, Index3D(256, 256, 256))
    val discreteImg = Resample.sample[Double](img, discreteDomain, outsideValue)
    show(discreteImg)
  }

  def show(model: StatisticalMeshModel) {
    VTKStatmodelViewer(model).main(Array(""))
  }

  // testing purpose
  def main(args: Array[String]): Unit = {
    smptk.initialize()
    val model = StatismoIO.readStatismoMeshModel(new java.io.File("/tmp/facemodel.h5")).get
    //val model = MeshIO.readMesh(new java.io.File("/tmp/facemesh.h5")).get
    show(model)
  }

}

