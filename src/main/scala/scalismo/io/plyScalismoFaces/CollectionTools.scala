package scalismo.io.plyScalismoFaces

import scala.collection.LazyZip3

object CollectionTools {

  def zip3[A, B, C](a: Seq[A], b: Seq[B], c: Seq[C]): Iterable[(A, B, C)] = {
    LazyZip3.lazyZip3ToIterable(a.lazyZip(b).lazyZip(c))
  }

}
