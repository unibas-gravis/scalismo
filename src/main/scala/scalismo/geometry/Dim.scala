package scalismo.geometry

/** a marker trait only meant to distinguish the dimension */
sealed trait Dim

trait _1D extends Dim
trait _2D extends Dim
trait _3D extends Dim

trait NDSpace[D <: Dim] {
  def dimensionality: Int
}

object Dim {
  implicit object OneDSpace extends NDSpace[_1D] {
    override val dimensionality = 1
  }
  implicit object TwoDSpace extends NDSpace[_2D] {
    override val dimensionality = 2
  }
  implicit object ThreeDSpace extends NDSpace[_3D] {
    override val dimensionality = 3
  }

}
