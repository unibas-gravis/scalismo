package scalismo.io.statisticalmodel

trait ObjectToArrayCast[A] {
  def cast(arr: Object): Array[A]
}

object ObjectToArrayCast {
  implicit object ObjectToStringArrayCast extends ObjectToArrayCast[String] {
    override def cast(arr: Object): Array[String] = {
      if (arr.isInstanceOf[Array[Array[String]]]) {
        arr.asInstanceOf[Array[Array[String]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[String]]]]) {
        arr.asInstanceOf[Array[Array[Array[String]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[String]]
      }
    }
  }

  implicit object ObjectToFloatArrayCast extends ObjectToArrayCast[Float] {
    override def cast(arr: Object): Array[Float] = {
      if (arr.isInstanceOf[Array[Array[Float]]]) {
        arr.asInstanceOf[Array[Array[Float]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Float]]]]) {
        arr.asInstanceOf[Array[Array[Array[Float]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Float]]
      }
    }
  }

  implicit object ObjectToByteArrayCast extends ObjectToArrayCast[Byte] {
    override def cast(arr: Object): Array[Byte] = {
      if (arr.isInstanceOf[Array[Array[Byte]]]) {
        arr.asInstanceOf[Array[Array[Byte]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Byte]]]]) {
        arr.asInstanceOf[Array[Array[Array[Byte]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Byte]]
      }
    }
  }

  implicit object ObjectToIntArrayCast extends ObjectToArrayCast[Int] {
    override def cast(arr: Object): Array[Int] = {
      if (arr.isInstanceOf[Array[Array[Int]]]) {
        arr.asInstanceOf[Array[Array[Int]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Int]]]]) {
        arr.asInstanceOf[Array[Array[Array[Int]]]].flatten.flatten
      } else {
        if (arr.isInstanceOf[Array[Array[Long]]])
          arr.asInstanceOf[Array[Array[Long]]].flatten.map(_.toInt)
        else
          arr.asInstanceOf[Array[Int]]
      }
    }
  }

  implicit object ObjectToDoubleArrayCast extends ObjectToArrayCast[Double] {
    override def cast(arr: Object): Array[Double] = {
      if (arr.isInstanceOf[Array[Array[Double]]]) {
        arr.asInstanceOf[Array[Array[Double]]].flatten
      } else if (arr.isInstanceOf[Array[Array[Array[Double]]]]) {
        arr.asInstanceOf[Array[Array[Array[Double]]]].flatten.flatten
      } else {
        arr.asInstanceOf[Array[Double]]
      }
    }
  }
}
