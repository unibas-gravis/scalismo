package scalismo.mesh.decimate

import scalismo.geometry.EuclideanVector3D
import scalismo.mesh.TriangleCell

case class Triangle(
                     var cell: TriangleCell,
                     var n: EuclideanVector3D = EuclideanVector3D(0.0, 0.0, 0.0),
                     var err: ErrorEntry = ErrorEntry(),
                     var deleted: Boolean = false,
                     var dirty: Boolean = false
                   )
