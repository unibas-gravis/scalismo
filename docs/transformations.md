# Transformations and Transformationspaces

### Transformations
Transformations are functions, 
which map a set of points of a domain $$D \subset \mathbb{R}^d$$ 
 to other points in $$\mathbb{R}^d$$.  
 
A transformation is defined as a special case of field. 
```scala
trait Transformation[D] extends Field[D, Point[D]] 
```

We can create a transformation by specifying a function ```f : Point[D] => Point[D]```
and its domain:

```scala mdoc 
import scalismo.common._
import scalismo.geometry._
import scalismo.transformations._
val domain : Domain[_3D] = EuclideanSpace[_3D]
Transformation3D(domain, p => p + EuclideanVector3D(1, 0, 0))
```
If we don't specify the domain, it is assumed that the transformation
is defined on the entire EuclideanSpace.

An important subclass of transformations are ```ParametricTransformations```. 
Parametric tranformations are completely defined by a finite set of parameters. 

An example of a parametric transformation is a translation:
```scala mdoc 
val translation = Translation3D(EuclideanVector3D(1, 3, 0.5))
translation.parameters
```

Another example is the rotation. To define a rotation, we specify not only the Euler angles, 
but also its rotation center. 
```scala mdoc
val rotation = Rotation3D(0, 0.1, 0.3, Point3D(0, 0, 0))
```

 ### Properties of Transformations
 
All parametric transformations are assumed to be differentiable with respect to the 
parameters. Transformations can have two additional properties. 
They can be invertible and differentiable with respect to the position in space. Invertible and differentiable
can be defined by adding the trait ```CanDifferentiateWRTPosition[D]``` or ```CanInvert[D, T]```
to the definition. 
 
If a transformation is invertible (such as e.g. a translation), 
we can get its inverse as follows:
```scala  mdoc
val tInvertible : Transformation[_3D] with CanInvert[_3D, Transformation] = Translation3D(EuclideanVector3D(1, 0, 0))
val tinv : Translation[_3D] = tInvertible.inverse
``` 
 
Similarly, we can get the derivative:
```scala  mdoc 
val tDifferentiable : Transformation[_3D] with CanDifferentiateWRTPosition[_3D] = Translation3D(EuclideanVector3D(1, 0, 0))
val tDerivative : Point[_3D] => SquareMatrix[_3D] = tDifferentiable.derivativeWRTPosition
```
 
### Transformation Spaces
 
Any parametric transformation has attached a corresponding transformation space. 
A TransformatonSpace object is a factory, which can be used to create for a 
given transformation the identity transformation or a transformation for a fixed set
of parameters. 

Let's take again the example of a translation. Translations in 3D are defined
by the translation vector $$(t = (t_1, t_2, t_3))$$. We define a corresponding
translation space as follows 
```scala mdoc
val translationSpace = TranslationSpace3D
```
The main usage of Transformation spaces is for optimization, namely, when 
we search for a transformation, which is optimal for some cost function. 
For this purpose, the following methods are useful:

* We can obtain the corresponding transformation by specifying parameters:
```scala mdoc
import breeze.linalg.DenseVector
val parameters = DenseVector(1.0, 0.0, 3.0)
val translationFromParameters = translationSpace.transformationForParameters(parameters)
val ident = translationSpace.identityTransformation
```


### Combining transformations 

Two transformations $$t_O, t_I$$  can be composed to obtain a ```CompositeTransformation``` $$t_O(t_I(x))$$.
```scala mdoc
val rotationThenTranslation = CompositeTransformation(translation, rotation)
``` 
If we want that the resulting transformation can be differentiated with respect to the position, we can write instead
```scala mdoc
val rotationThenTranslationDifferentiable = CompositeDifferentiableTransformation(translation, rotation)
``` 

The corresponding transformation space is called a ```ProductTransformationSpace``` and ```ProductTransformationSpaceWithDifferentiableTransforms```.
We can define the following:
```scala mdoc
val compositeSpace = ProductTransformationSpace(TranslationSpace3D, RotationSpace3D(Point3D(0, 0, 0)))
val ct : CompositeTransformation[_3D, Translation, Rotation] = compositeSpace.transformationForParameters(DenseVector(0.0 ,0.0 ,0.0, 0.0, 0.0, 0.0))
``` 

### Rigid and similarity transformations
In shape modelling, rigid and similarity transformations play a special role. Scalismo therefore already provides special 
classes for these. We have the following classes:
* ```RotationThenTranslation```
* ```TranslationThenRotation```
* ```RotationThenScalingThenTranslation```
* ```TranslationThenScalingThenRotation```
The first two are rigid transformations, the second two are similarity transformations. The two transformations in each group
are inverses of each other:
```scala mdoc
val rinv : TranslationThenRotation[_3D] = RotationThenTranslation(rotation, translation).inverse
```
