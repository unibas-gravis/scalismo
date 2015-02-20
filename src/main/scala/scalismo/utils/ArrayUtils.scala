package scalismo.utils

import scala.reflect.ClassTag

object ArrayUtils {
  def fastMap[In, Out: ClassTag](in: Array[In], f: In => Out): Array[Out] = {
    val length = in.length
    val out = Array.ofDim[Out](length)
    var index = 0
    while (index < length) {
      out(index) = f(in(index))
      index += 1
    }
    out
  }
}
