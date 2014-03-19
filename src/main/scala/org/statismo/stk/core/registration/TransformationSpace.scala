package org.statismo.stk.core
package registration

import TransformationSpace.ParameterVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import org.statismo.stk.core.geometry._


trait ValueCaching[D <: Dim] extends (Point[D] => Point[D]) {
  
  val cache =  scala.collection.mutable.HashMap.empty[Point[D], Point[D]]

  abstract override def apply(p: Point[D]): Point[D] = {
    cache.getOrElseUpdate(p,  super.apply(p))
  } 
} 

trait TransformationSpaceConfiguration {
  val withValueCaching : Boolean
}


trait Transformation[D <: Dim] extends (Point[D] => Point[D]) {
  def takeDerivative(x: Point[D]): MatrixNxN[D]
}

trait CanInvert[D <: Dim] { self : Transformation[D] =>
  def inverse : Transformation[D]
}

trait TransformationSpace[D <: Dim]  {
  self: TransformationSpace[D] =>


  type ConcreteTransformation <: Transformation[D]


  type JacobianImage = Function1[Point[D], DenseMatrix[Float]]
  def parametersDimensionality: Int
  def takeDerivativeWRTParameters(alpha: ParameterVector): JacobianImage

  def transformForParameters(p : ParameterVector) : ConcreteTransformation
  def apply(p : ParameterVector) : ConcreteTransformation = transformForParameters(p)

  def product(that: TransformationSpace[D]): TransformationSpace[D] = {
    new ProductTransformationSpace(self, that)
  }

  def identityTransformParameters: DenseVector[Float]
}



class ProductTransformationSpace[D <: Dim, OuterType <: TransformationSpace[D], InnerType <: TransformationSpace[D]](outer: OuterType, inner: InnerType) extends TransformationSpace[D] {

  def parametersDimensionality = outer.parametersDimensionality + inner.parametersDimensionality
  def identityTransformParameters = DenseVector.vertcat(outer.identityTransformParameters, inner.identityTransformParameters)

  class ProductTransformation(outerTransform : OuterType#ConcreteTransformation, innerTransform : InnerType#ConcreteTransformation) extends Transformation[D] {
    override def apply(x : Point[D]) = {
      (outerTransform compose innerTransform)(x)
    }

    override def takeDerivative(x: Point[D]) = {
      outerTransform.takeDerivative(innerTransform(x)) * innerTransform.takeDerivative(x)
    }

  }

  override type ConcreteTransformation = ProductTransformation


  override def transformForParameters(p : ParameterVector) = {
    val (outerParams, innerParams)  = splitProductParameterVector(p)
    new ProductTransformation(outer.transformForParameters(outerParams) , inner.transformForParameters(innerParams))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {

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

case class TranslationSpace1D() extends TransformationSpace[OneD] {    self =>

  class TranslationTransform1D(p : ParameterVector) extends Transformation[OneD] with CanInvert[OneD] {
    def apply(pt: Point[OneD]) : Point[OneD] = Point1D(p(0).toFloat + pt(0))
    override def takeDerivative(x: Point[OneD]) : Matrix1x1 = {
      Matrix1x1.eye
    }
    override def inverse : TranslationTransform1D = self.transformForParameters(-p)
  }

  override type ConcreteTransformation = TranslationTransform1D

  def parametersDimensionality: Int = 1
  override def identityTransformParameters = DenseVector(0.0f, 0.0f)

  override def transformForParameters(p: ParameterVector) : TranslationTransform1D = {
    new TranslationTransform1D(p)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[OneD] =>
    DenseMatrix.eye[Float](1)
  }
}

case class TranslationSpace2D() extends TransformationSpace[TwoD] { self =>

  class TranslationTransform2D(p : ParameterVector) extends Transformation[TwoD] with CanInvert[TwoD] {
    def apply(pt: Point[TwoD]) : Point[TwoD] = Point2D(p(0).toFloat + pt(0), p(1).toFloat + pt(1))
    override def takeDerivative(x: Point[TwoD]) : Matrix2x2 = {
      Matrix2x2.eye
    }
    override def inverse : TranslationTransform2D = self.transformForParameters(-p)
  }

  override type ConcreteTransformation = TranslationTransform2D

  def parametersDimensionality: Int = 2
  override def identityTransformParameters = DenseVector(0.0f, 0.0f)
  
  override def transformForParameters(p: ParameterVector) : TranslationTransform2D = {
    new TranslationTransform2D(p)
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
    DenseMatrix.eye[Float](2)
  }
}

case class TranslationSpace3D() extends TransformationSpace[ThreeD] { self =>


  class TranslationTransform3D(p : ParameterVector) extends Transformation[ThreeD] with CanInvert[ThreeD] {
    def apply(pt: Point[ThreeD]) : Point[ThreeD] = Point3D(p(0).toFloat + pt(0), p(1).toFloat + pt(1), p(2).toFloat + pt(2))
    def takeDerivative(x: Point[ThreeD]) : MatrixNxN[ThreeD] = {
      val m : MatrixNxN[ThreeD] = Matrix3x3.eye
      m
    }

    override def inverse : TranslationTransform3D = self.transformForParameters(-p)
  }

  override type ConcreteTransformation = TranslationTransform3D

  def parametersDimensionality: Int = 3
  override def identityTransformParameters = DenseVector(0f, 0f, 0f)

  override def transformForParameters(p : ParameterVector) : ConcreteTransformation  = new TranslationTransform3D(p)

  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
    DenseMatrix.eye[Float](3)
  }
}


case class RotationSpace3D(val centre: Point[ThreeD]) extends TransformationSpace[ThreeD] { self =>

  def parametersDimensionality: Int = 3 //  Euler angles 
  override def identityTransformParameters = DenseVector(0f, 0f, 0f)

  def rotationParametersToParameterVector(phi: Double, theta: Double, psi: Double): ParameterVector = {
    DenseVector(phi.toFloat, theta.toFloat, psi.toFloat)
  }


  class RotationTransform3D(rotMatrix : MatrixNxN[ThreeD]) extends Transformation[ThreeD] with CanInvert[ThreeD] {
    def apply(pt: Point[ThreeD]) : Point[ThreeD] = {
      val ptCentered = pt - centre
      val rotCentered = rotMatrix * ptCentered
      centre + Vector3D(rotCentered(0).toFloat, rotCentered(1).toFloat, rotCentered(2).toFloat)
    }
    def takeDerivative(x: Point[ThreeD]) : MatrixNxN[ThreeD] = {
      rotMatrix
    }

    override def inverse : RotationTransform3D = {
      new RotationTransform3D(MatrixNxN.inv(rotMatrix))
    }
  }

  override type ConcreteTransformation = RotationTransform3D


  override def transformForParameters(p: ParameterVector) : RotationTransform3D = {
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
        
    new RotationTransform3D(rotMatrix)
  }


  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[ThreeD] =>
 
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

case class RotationSpace2D(val centre: Point[TwoD]) extends TransformationSpace[TwoD] {       self =>



  class RotationTransform2D(rotMatrix : MatrixNxN[TwoD]) extends Transformation[TwoD] with CanInvert[TwoD] {
    def apply(pt: Point[TwoD]) : Point[TwoD] = {
      val ptCentered = pt - self.centre
      val rotCentered = rotMatrix * ptCentered
      centre + Vector2D(rotCentered(0).toFloat, rotCentered(1).toFloat)

    }
    def takeDerivative(x: Point[TwoD]) : MatrixNxN[TwoD] = {
      rotMatrix
    }

    override def inverse : RotationTransform2D = {
      new RotationTransform2D(MatrixNxN.inv(rotMatrix))
    }
  }

  override type ConcreteTransformation = RotationTransform2D


  def parametersDimensionality: Int = 1 //  angle
  override def identityTransformParameters = DenseVector(0f)
  


  def rotationParametersToParameterVector(phi: Double): ParameterVector = {
    DenseVector(phi.toFloat)
  }
  override def transformForParameters(p: ParameterVector) : ConcreteTransformation= {
    require(p.length == 1)

    val rotMatrix = Matrix2x2(
        (math.cos(p(0)).toFloat, -math.sin(p(0)).toFloat), 
        (math.sin(p(0)).toFloat, math.cos(p(0)).toFloat)
        )
    new RotationTransform2D(rotMatrix)
  }


  override def takeDerivativeWRTParameters(p: ParameterVector) = { x: Point[TwoD] =>
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


  class ScalingTransformation3D(s : Float) extends Transformation[ThreeD] with CanInvert[ThreeD] {
    def apply(x: Point[ThreeD]) : Point[ThreeD] =  Point3D(x(0) * s, x(1) *s, x(2) * s)
    def takeDerivative(x: Point[ThreeD]) : MatrixNxN[ThreeD] = Matrix3x3.eye * s

    override def inverse : ScalingTransformation3D = {
      if (s == 0) new ScalingTransformation3D(0) else new ScalingTransformation3D( 1.0f / s)
    }
  }

  override type ConcreteTransformation = ScalingTransformation3D


  def parametersDimensionality: Int = 1
  override def identityTransformParameters = DenseVector(1f)
  override def transformForParameters (p: ParameterVector) : ConcreteTransformation = {
    require(p.length == 1)
    new ScalingTransformation3D(p(0))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[ThreeD] => DenseMatrix((x(0)), (x(1)), (x(2)))
  }
}

case class ScalingSpace2D() extends TransformationSpace[TwoD] {


  class ScalingTransformation2D(s : Float) extends Transformation[TwoD] with CanInvert[TwoD] {
    def apply(x: Point[TwoD]) : Point[TwoD] =  Point2D(x(0) * s, x(1) *s)
    def takeDerivative(x: Point[TwoD]) : MatrixNxN[TwoD] = Matrix2x2.eye * s

    override def inverse : ScalingTransformation2D = {
      if (s == 0) new ScalingTransformation2D(0) else new ScalingTransformation2D( 1.0f / s)
    }
  }

  override type ConcreteTransformation = ScalingTransformation2D


  def parametersDimensionality: Int = 1
  override def identityTransformParameters = DenseVector(1f)
  override def transformForParameters (p: ParameterVector) : ConcreteTransformation = {
    require(p.length == 1)
    new ScalingTransformation2D(p(0))
  }

  override def takeDerivativeWRTParameters(p: ParameterVector) = {
    x: Point[TwoD] => DenseMatrix((x(0)), (x(1)))
  }
}


case class RigidTransformationSpace3D(center : Point[ThreeD] = Point3D(0,0,0))
  extends ProductTransformationSpace[ThreeD, TranslationSpace3D, RotationSpace3D](TranslationSpace3D(), RotationSpace3D(center)) {

  val invRigidTransformation = new ProductTransformationSpace[ThreeD, RotationSpace3D, TranslationSpace3D](RotationSpace3D(center), TranslationSpace3D())

  class RigidTransformation3D(translationTransform : TranslationSpace3D#TranslationTransform3D, rotationTransform : RotationSpace3D#RotationTransform3D)
    extends ProductTransformation(translationTransform, rotationTransform) with CanInvert[ThreeD] {

    // TODO the type of the inverse is not as tight as  it could be
    def inverse  = new invRigidTransformation.ProductTransformation(rotationTransform.inverse, translationTransform.inverse)
  }

}



case class RigidTransformationSpace2D(center : Point[TwoD] = Point2D(0,0))
  extends ProductTransformationSpace[TwoD, TranslationSpace2D, RotationSpace2D](TranslationSpace2D(), RotationSpace2D(center)) {

  val invRigidTransformation = new ProductTransformationSpace[TwoD, RotationSpace2D, TranslationSpace2D](RotationSpace2D(center), TranslationSpace2D())

  class RigidTransformation2D(translationTransform : TranslationSpace2D#TranslationTransform2D, rotationTransform : RotationSpace2D#RotationTransform2D)
    extends ProductTransformation(translationTransform, rotationTransform) with CanInvert[TwoD] {

    // TODO the type of the inverse is not as tight as  it could be
    def inverse  = new invRigidTransformation.ProductTransformation(rotationTransform.inverse, translationTransform.inverse)
  }

}

object TransformationSpace {
  type ParameterVector = DenseVector[Float]

}