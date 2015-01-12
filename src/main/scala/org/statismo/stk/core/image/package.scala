package org.statismo.stk.core


/**
 * Contains classes for representing discrete and continuous images as well as filters for filtering both types of images.
 *
 * In this library, the standard type of images are just functions, defined on some domain.
 * There is also the more traditional discrete image type, which represents image data on a regular pixel/voxel grid.
 *
 * DiscreteImages are mainly used for reading/writing and simple manipulation of image values. The continuous representation
 * of images is much more flexible and most method for manipulating images are defined only on this image type.
 * A discrete image can be converted to a continuous image by using an interpolation procedure:
 * {{{
    val domain = DiscreteImageDomain(Point(0,0), Vector(1,1), Index(255,255))
    val di = DiscreteScalarImage(domain)(0)
    val discreteImage =  DiscreteScalarImage(domain, (_ : Point[_2D]) => 1.0f)
    val continuousImage = discreteImage.interpolate(3)
 * }}}
 *
 * To get back the discrete representation, we can sample the iamge values on a regular grid:
 * {{{
 *  val newDomain = DiscreteImageDomain(Point(0,0), Vector(1,1), Index(128,128))
 *  val resampledDiscreteImage = continuousImage.sample(domain, 0)
 * }}}
 */
package object image {

}
