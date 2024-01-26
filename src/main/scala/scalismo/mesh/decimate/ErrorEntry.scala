package scalismo.mesh.decimate

case class ErrorEntry(vertexError: Array[Double] = Array.fill(3)(0.0), var minVertexError: Double = 0.0)
