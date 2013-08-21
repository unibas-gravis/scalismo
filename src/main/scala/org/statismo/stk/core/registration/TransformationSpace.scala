package smptk
package registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import smptk.geometry._


trait ValueCaching[D <: Dim] extends (Point[D] => Point[D]) {
  
  val cache =  scala.collection.mutable.HashMap.empty[Point[D], Point[D]]

  abstract override def apply(p: Point[D]): Point[D] = {
    cache.getOrElseUpdate(p,  super.apply(p))
  } 
} 

trait TransformationSpaceConfiguration {
  val withValueCaching : Boolean
}

trait TransformationSpace[D <: Dim] extends Function1[ParameterVector, Transformation[D]] {
  self: TransformationSpace[D] =>

  type JacobianImage = Function1[Point[D], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage

  def product(that: TransformationSpace[D]): TransformationSpace[D] = {
    new ProductTransformationSpace(self, that)
  }

  def inverseTransform(p: ParameterVector): Option[Transformation[D]]
  def identityTransformParameters: DenseVector[Float]
}

trait Transformation[D <: Dim] extends (Point[D] => Point[D]) {
  def takeDerivative(x: Point[D]): MatrixNxN[D]
}


class ProductTransformationSpace[D <: Dim, OuterType <: TransformationSpace[D], InnerType <: TransformationSpace[D]](outer: OuterType, inner: InnerType) extends TransformationSpace[D] {

  def parametersDimensionality = outer.parametersDimensionality + inner.parametersDimensionality
  def identityTransformParameters = DenseVector.vertcat(outer.identityTransformParameters, inner.identityTransformParameters)

  def apply(p: ParameterVector) = {

    new Transformation[D] {
      def apply(x: Point[D]) = {
        val (pThis, pThat) = splitProductParameterVector(p)
        val outerTransform = outer(pThis)
        val innerTransfrom = inner(pThat)
        (outerTransform compose innerTransfrom)(x)
      }
      def takeDerivative(x: Point[D]) = {
        val (pThis, pThat) = splitProductParameterVector(p)
        val outerTransform = outer(pThis)
        val innerTransform = inner(pThat)
        outerTransform.takeDerivative(innerTransform(x)) * innerTransform.takeDerivative(x)
      }
    }
  }

  def inverseTransform(p: ParameterVector): Option[Transformation[D]] = {
    val (pOuter, pInner) = splitProductParameterVector(p)

    for {
      outerInverse <- outer.inverseTransform(pOuter);
      innerInverse <- inner.inverseTransform(pInner)
    } yield {
      new Transformation[D] {
        def apply(x: Point[D]) = (innerInverse compose outerInverse)(x)
        def takeDerivative(x: Point[D]) = {
          innerInverse.takeDerivative(outerInverse(x)) * outerInverse.takeDerivative(x)
        }
      }
    }

  }

  def takeDerivativeWRTParameters(p: ParameterVector) = {

    val split = splitProductParameterVector(p)

    (x: Point[D]) => {
      DenseMatrix.horzcat(
        outer.takeDerivativeWRTParameters(split._1)(x),
        outer(split._1).takeDerivative(inner(split._2)(x)).toBreezeMatrix * inner.takeDerivativeWRTParameters(split._2)(x))
    }
  }

  private def splitProductParameterVector(p: ParameterVector): (ParameterVector, ParameterVector) = {
    val pThis = p.slice(0, outer.parametersDimensionality, 1)
    val pThat = p.slice(outer.parametersDimensionality, p.length, 1)
    (pThis, pThat)
  }

}

case class TranslationSpace1D() extends TransformationSpace[OneD] {
  
  override def identityTransformParameters = DenseVector(0.0f)
  
  def apply(p: ParameterVector) = {
    new Transformation[OneD] {
      def apply(pt: Point[OneD]) : Point[OneD] = Point1D(p(0).toFloat + pt(0))
      def takeDerivative(x: Point[OneD]) = {
        Matrix1x1.eye
      }
    }
  }
  def inverseTransform(p: ParameterVector) = {
    Some(TranslationSpace1D()(-p))
  }

  def parametersDimensionality: Int = 1
  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[OneD] =>
    DenseMatrix.eye[Float](1)
  }
}

case class TranslationSpace2D() extends TransformationSpace[TwoD] {

  
  def parametersDimensionality: Int = 2
  override def identityTransformParameters = DenseVector(0.0f, 0.0f)
  
  def apply(p: ParameterVector) = {
    new Transformation[TwoD] {
      def apply(pt: Point[TwoD]) : Point[TwoD] = Point2D(p(0).toFloat + pt(0), p(1).toFloat + pt(1))
      def takeDerivative(x: Point[TwoD]) = {
        Matrix2x2.eye
      }
    }
  }
  def inverseTransform(p: ParameterVector) = {
    Some(TranslationSpace2D()(-p))
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
    DenseMatrix.eye[Float](2)
  }
}

case class TranslationSpace3D() extends TransformationSpace[ThreeD] {
  
  def parametersDimensionality: Int = 3
  override def identityTransformParameters = DenseVector(0f, 0f, 0f)
  
  def apply(p: ParameterVector) = {
    new Transformation[ThreeD] {
      def apply(pt: Point[ThreeD]) : Point[ThreeD] = Point3D(p(0).toFloat + pt(0), p(1).toFloat + pt(1), p(2).toFloat + pt(2))
      def takeDerivative(x: Point[ThreeD]) = {
        Matrix3x3.eye
      }
    }
  }
  def inverseTransform(p: ParameterVector) = {
    Some(TranslationSpace3D()(-p))
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
    DenseMatrix.eye[Float](3)
  }
}


case class RotationSpace3D(val centre: Point[ThreeD]) extends TransformationSpace[ThreeD] {

  def parametersDimensionality: Int = 3 //  Euler angles 
  override def identityTransformParameters = DenseVector(0f, 0f, 0f)

  def rotationParametersToParameterVector(phi: Double, theta: Double, psi: Double): ParameterVector = {
    DenseVector(phi.toFloat, theta.toFloat, psi.toFloat)
  }
  def apply(p: ParameterVector) = {
    require(p.length == 3) 
    //
    // rotation matrix according to the "x-convention" where Phi is rotation over x-axis, theta over y, and psi over z
    val cospsi = Math.cos(p(2)).toFloat
    val sinpsi = Math.sin(p(2)).toFloat
   
    val costh = Math.cos(p(1)).toFloat
    val sinth = Math.sin(p(1)).toFloat
    
    val cosphi = Math.cos(p(0)).toFloat
    val sinphi = Math.sin(p(0)).toFloat
    
    val rotMatrix = Matrix3x3(
         (costh*cosphi , sinpsi*sinth*cosphi-cospsi*sinphi , sinpsi*sinphi+cospsi*sinth*cosphi),
        (costh*sinphi , cospsi*cosphi+sinpsi*sinth*sinphi , cospsi*sinth*sinphi-sinpsi*cosphi),
        (-sinth, sinpsi*costh, cospsi*costh)
        ) 
        
        
 //   println("Rotation matrix in transform "+rotMatrix)     
        
    new Transformation[ThreeD] {
      def apply(pt: Point[ThreeD]) : Point[ThreeD]= {
        val ptCentered = pt - centre 
        val rotCentered = rotMatrix * ptCentered
         centre + Vector3D(rotCentered(0).toFloat, rotCentered(1).toFloat, rotCentered(2).toFloat)

      }
      def takeDerivative(x: Point[ThreeD]) = {
        rotMatrix
      }
    }
  }

  def inverseTransform(p: ParameterVector) = {
    Some(RotationSpace3D(centre)(-p))
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
 
    val cospsi = Math.cos(p(2))
    val sinpsi = Math.sin(p(2)) 
    val costh = Math.cos(p(1))
    val sinth = Math.sin(p(1))
    val cosphi = Math.cos(p(0))
    val sinphi = Math.sin(p(0))
    
    val x0minc0 = x(0)-centre(0)
    val x1minc1 = x(1)-centre(1)
    val x2minc2 = x(2)-centre(2)

    // 3 by 3 matrix (nbrows=point dim, nb cols = param dim )
    val dr00 = (-sinphi * costh *x0minc0)+(-sinphi*sinpsi*sinth - cospsi*cosphi)*x1minc1 + (sinpsi*cosphi-cospsi*sinth*sinphi)*x2minc2
    val dr01 = -sinth*cosphi*x0minc0 + costh*sinpsi*cosphi*x1minc1 + cospsi*costh*cosphi*x2minc2
    val dr02 = (cospsi*sinth*cosphi + sinpsi*sinphi) *x1minc1 + (cospsi*sinphi-sinpsi*sinth*cosphi)*x2minc2
    
    val dr10 = costh*cosphi*x0minc0 + (-sinphi*cospsi + sinpsi*sinth*cosphi)*x1minc1 + (cospsi*sinth*cosphi+sinpsi*sinphi)*x2minc2
    val dr11 = -sinth*sinphi*x0minc0 + sinpsi*costh*sinphi*x1minc1 + cospsi*costh*sinphi*x2minc2
    val dr12 = (-sinpsi*cosphi+cospsi*sinth*sinphi)*x1minc1 + (-sinpsi*sinth*sinphi-cospsi*cosphi)*x2minc2
    
    val dr20 = 0.0
    val dr21 = -costh*x0minc0-sinpsi*sinth*x1minc1-cospsi*sinth*x2minc2
    val dr22 = cospsi*costh*x1minc1 -sinpsi*costh*x2minc2
    
//    val dr00 = (cospsi*sinth*cosphi+sinpsi*sinphi)*x1minc1+(cospsi*sinphi-sinpsi*sinth*cosphi)*x2minc2
//    val dr01 = (-sinth*cosphi*x0minc0 + costh*cosphi*sinpsi*x1minc1 + cospsi*costh*cosphi* x2minc2)
//    val dr02 = (-sinphi*costh*x0minc0)+ (-sinphi*sinth*sinpsi - cospsi*cosphi) * x1minc1 + (cosphi*sinpsi-sinphi*sinth*cospsi)* x2minc2
//    val dr10 = (-sinpsi*cosphi+cospsi*sinth*sinphi)*x1minc1+ (-sinpsi*sinth*sinphi-cospsi*cosphi)*x2minc2
//    val dr11 = (-sinth*sinphi*x0minc0 + costh*sinpsi*sinphi*x1minc1+cospsi*costh*sinphi*x2minc2)
//    val dr12 = costh*cosphi*x0minc0+(-sinphi*cospsi+cosphi*sinpsi*sinth)*x1minc1+(cosphi*sinth*cospsi+sinpsi*sinphi)*x2minc2
//    val dr20 = cospsi*costh*x1minc1
//    val dr21 = -costh*x0minc0 + -sinth*sinpsi*x1minc1 + (-sinth*cospsi)*x2minc2
//    val dr22 = 0.
    
    DenseMatrix(
      (dr00, dr01, dr02),        
      (dr10 , dr11, dr12),     
      (dr20  , dr21 ,dr22)
    ).map(_.toFloat)
  }
}

case class RotationSpace2D(val centre: Point[TwoD]) extends TransformationSpace[TwoD] {

  def parametersDimensionality: Int = 1 //  angle
override def identityTransformParameters = DenseVector(0f)	
  
  def rotationParametersToParameterVector(phi: Double): ParameterVector = {
    DenseVector(phi.toFloat)
  }
  def apply(p: ParameterVector) = {
    require(p.length == 1)

    val rotMatrix = Matrix2x2(
        (math.cos(p(0)).toFloat, -math.sin(p(0)).toFloat), 
        (math.sin(p(0)).toFloat, math.cos(p(0)).toFloat)
        )

    new Transformation[TwoD] {
      def apply(pt: Point[TwoD]) : Point[TwoD] = {

        val ptCentered = pt - centre
        val rotCentered = rotMatrix * ptCentered
        centre + Vector2D(rotCentered(0).toFloat, rotCentered(1).toFloat) 

      }
      def takeDerivative(x: Point[TwoD]) = {
        rotMatrix
      }
    }
  }

  def inverseTransform(p: ParameterVector) = {
    Some(RotationSpace2D(centre)(-p))
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
    val sa = math.sin(p(0))
    val ca = math.cos(p(0))
    val cx = centre(0)
    val cy = centre(1)

    DenseMatrix(
      (-sa * (x(0) - cx) - ca * (x(1) - cy)),
      (ca * (x(0) - cx) - sa * (x(1) - cy))
      ).map(_.toFloat)
  }
}

case class ScalingSpace3D() extends TransformationSpace[ThreeD] {

  def parametersDimensionality: Int = 1
    override def identityTransformParameters = DenseVector(1f)
  def apply(p: ParameterVector) = {
    require(p.length == 1)

    new Transformation[ThreeD] {
      def apply(x: Point[ThreeD]) : Point[ThreeD] = {
        val s = p(0).toFloat
        Point3D(x(0) * s, x(1) *s, x(2) * s)
      }

      def takeDerivative(x: Point[ThreeD]) = {
        Matrix3x3.eye * p(0)
      }
    }
  }

  def inverseTransform(p: ParameterVector) = {
    if (p(0) == 0) {
      throw new Exception("Inverse transfrom of scaling by 0 not allowed !!")
      None
    } else
      Some(ScalingSpace3D()(DenseVector(1 / p(0))))
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[ThreeD] => DenseMatrix((x(0)), (x(1)), (x(2)))
  }
}

case class ScalingSpace2D() extends TransformationSpace[TwoD] {

  def parametersDimensionality: Int = 1
    override def identityTransformParameters = DenseVector(1f)
  def apply(p: ParameterVector) = {
    require(p.length == 1)

    new Transformation[TwoD] {
      def apply(x: Point[TwoD]) : Point[TwoD]= {
        val s = p(0).toFloat
        Point2D(x(0) * s, x(1) * s)
      }

      def takeDerivative(x: Point[TwoD]) = {
        Matrix2x2.eye * p(0)
      }
    }
  }

  def inverseTransform(p: ParameterVector) = {
    if (p(0) == 0) {
      throw new Exception("Inverse transfrom of scaling by 0 not allowed !!")
      None
    } else
      Some(ScalingSpace2D()(DenseVector(1 / p(0))))
  }

  def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[TwoD] => DenseMatrix((x(0)), (x(1)))
  }
}

case class RigidTransformationSpace3D(center: Point[ThreeD])
  extends ProductTransformationSpace[ThreeD, TranslationSpace3D, RotationSpace3D](TranslationSpace3D(), RotationSpace3D(center)) {

}

case class RigidTransformationSpace2D(center: Point[TwoD])
  extends ProductTransformationSpace[TwoD, TranslationSpace2D, RotationSpace2D](TranslationSpace2D(), RotationSpace2D(center)) {

}

object TransformationSpace {
  type ParameterVector = DenseVector[Float]

}