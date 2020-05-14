# Images 

```scala mdoc:invisible
import scalismo.common._
import scalismo.geometry._
import scalismo.image._
import scalismo.common.interpolation._
```


Similar to other types of representations, images come in two types: Discrete images and continuous images. 
Continuous images are just Fields, i.e. they are functions that have a domain $$D \subset \mathbb{R}^d$$ and 
map each point of the domain to some values. The mapped values can be scalars, vectors or even more complicated objects. 
Discrete Images are more interesting. A discrete image is a discrete field, whose domain is constrained
to be a regular grid, i.e. whose domain points are equally spaced. That the points are equally spaces makes 
it possible to represent the domain points implicitly by a mathematical formula rather than having to explicitly store them. 
Also, we can access each value efficiently (in constant time). 

### Structured Points

The basic object to represent such a set of structured points is the class ```StructuredPoints```.
We can create a set of points on a grid as follows:
```scala mdoc:silent
val origin = Point3D(10.0, 15.0, 7.0) // some point defining the lower left corner of the image
val spacing = EuclideanVector3D(0.1, 0.1, 0.1) // the spacing between two grid points in each space dimension
val size = IntVector3D(32, 64, 92) // size in each space dimension

val points = StructuredPoints3D(origin, spacing, size)
```
This creates a grid of points $$32 \times 64 \times 92$$ points, where the bottom left point is at the ```origin```,
and the points are the in the $$x, y, z$$ direction $$0.1mm$$ apart. 

Note that the grid of points is aligned to the coordinate axis. In case you would like to have a different 
alignment, it is possible to specify a rotation of the points. The rotation is specified by 1 or 3 euler angles, 
depending on whether there is a 2 or 3 dimensional image. 
```scala mdoc:silent
val yaw = Math.PI / 2
val pitch = 0.0
val roll = Math.PI

val points2 = StructuredPoints3D(origin, spacing, size, yaw, pitch, roll)
```

### Image domain

The image domain represents a domain, whose points are aligned in a rectangular grid. 
Naturally, it uses ```StructuredPoints``` as a representation of the underlying points of the 
domain. Thus we could just create an imageDomain as follows:
```scala mdoc:silent
val imageDomain = DiscreteImageDomain3D(StructuredPoints3D(origin, spacing, size))
``` 

However, more often we simply specify directly the origin, spacing and size, as we did 
for the structured points. 
```scala mdoc:silent
val imageDomain2 = DiscreteImageDomain3D(origin, spacing, size)
```
As for structured points, we can also define a rotation, by specifying the corresponding
Euler angles. 
```scala mdoc:silent
val imageDomain3 = DiscreteImageDomain3D(origin, spacing, size, yaw, pitch, roll)
```

Finally, we can specify the points by specifying its bounding box together with the information about the spacing or size:
```scala mdoc:silent
val boundingBox : BoxDomain[_3D] = imageDomain.boundingBox
val imageDomain4 = DiscreteImageDomain3D(boundingBox, spacing = spacing)
```
or 
```scala mdoc:silent
val imageDomain5 = DiscreteImageDomain3D(boundingBox, size = size)
```

This last creation method is particularly useful for changing the resolution of an image, 
as we will see later.

### Images
To create an image, we need to specify a value for each 
domain points. In this example, we create an image which assigns a zero value to each image.
```scala mdoc:silent
val image = DiscreteImage3D(imageDomain, p => 0.toShort)
```
Alternatively, we could have specified the values using an array, as follows:
```scala mdoc:silent
val image2 = DiscreteImage3D(imageDomain, Array.fill(imageDomain.pointSet.numberOfPoints)(0.toShort))

```
Note that an image is just another name for a discrete field with a image domain. We could 
have equally well constructed the image as:
```scala mdoc:silent
val image3 = DiscreteField3D(imageDomain, p => 0.toShort)
```

### Interpolation and discretization.

It is often more convenient to work with a continuous representation of the image. 
To obtain a continous image, we use the ```interpolate``` Method and specify a suitable
interpolator:
```scala mdoc:silent
val continuousImage : Field[_3D, Short] = image.interpolate(BSplineImageInterpolator3D(degree = 3))
``` 
The resulting object is defined on all the points within the bounding box of the image domain. 
To go back to a discrete representation, we can specify a new image domain and use the 
Method ```discretize```. As the new domain could be bigger than the domain of the continuous image, 
we need to specify a value that is assigned to the points, which fall outside this domain.  
 
```scala mdoc:silent
val newDomain = DiscreteImageDomain3D(image.domain.boundingBox, size=IntVector(32, 32, 32))
val resampledImage : DiscreteImage[_3D, Short] = continuousImage.discretize(newDomain, outsideValue = 0.toShort)
``` 
