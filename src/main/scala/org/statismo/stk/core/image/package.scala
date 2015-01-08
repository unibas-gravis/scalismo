package org.statismo.stk.core


/**
 * Contains classes for representing discrete and continuous images as well as filters for filtering both types of images.
 *
 * The two main types of images are continuous images and discrete images.
 * $ Continuous images are continuous functions defined on an image domain (usually a closed region).
 * $ Discrete images are defined on a (rectangular) image grid, and defined for each grid node a pixel value.
 *
 * It is possible to convert between a discrete image and a continuous image by interpolation and sampling.
 * {{{
    val domain = DiscreteImageDomain(Point(0,0), Vector(1,1), Index(255,255))
    val di = DiscreteScalarImage(domain)(0)
    val discreteImage =  DiscreteScalarImage(domain, (_ : Point[_2D]) => 1.0f)
    val continuousImage = DiscreteScalarImage.interpolate(discreteImage, 3)
    val resampledDiscreteImage = ContinuousScalarImage.sample[_2D, Short](continuousImage, domain, 0)
 * }}}
 */
package object image {

}
