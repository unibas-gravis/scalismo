package scalismo.io.statisticalmodel

case class NDArray[T](dims: IndexedSeq[Long], data: Array[T]) {
  require(dims.product == data.length, s"${dims.product} != ${data.length}")
}
