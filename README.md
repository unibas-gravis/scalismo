# Scalismo - Scalable Image Analysis and Shape Modelling [![Build Status](https://travis-ci.org/unibas-gravis/scalismo.svg?branch=develop)](https://travis-ci.org/unibas-gravis/scalismo)

Scalismo is a library for statistical shape modeling and model-based image analysis in Scala, developed by the
[Graphics and Vision Research Group](http://gravis.cs.unibas.ch) at the [University of Basel](http://www.unibas.ch). It is based on the  the concept of [Probabilistic Morphable Models](https://gravis.dmi.unibas.ch/PMM/). 

The vision of the project is to provide an environment for modelling and image analysis which 

* makes it easy and fun to try out ideas and build research prototypes
* is powerful enough to build full-scale industrial applications
* makes it feasible to deploy it in complex, distributed imaging pipelines. 

We aim to achieve these properties by leveraging two core technologies:

* A simple but versatile [mathematical approach](http://ieeexplore.ieee.org/document/8010438/) to shape modeling and registration, based on the theory of Gaussian processes.
* The Scala and Java ecosystem for reducing the software complexity. 

## Documentation

* [Scalismo tutorials](https://unibas-gravis.github.io/scalismo-tutorial/)
* [API Doc](http://unibas-gravis.github.io/scalismo/latest/api/index.html)

#### More about the concepts and mathematical methods behind Scalismo
* [Online course on shape modelling (using scalismo)](http://shapemodelling.cs.unibas.ch) 
* [Probabilistic Morphable Models](https://gravis.dmi.unibas.ch/PMM/)



There is also a [scalismo google group](https://groups.google.com/forum/#!forum/scalismo) for general questions and discussions. 


## How can I help?
While scalismo is already fully usable for shape modeling and simple image processing task, its functionality is currently targeted
to support the needs that arise in the research at the Gravis and Vision research group. If you find that a feature is missing, please let us know about it by opening an issue and describing the missing feature. 

We welcome contributions to scalismo. Please check the [Contributor's guide](contributing.md) for instructions how to contribute to Scalismo. 

We are also always grateful if you report bugs or if give us feedback on how you use scalismo in your work and how you think we can improve it. 

## Maintainers
The project is currently developed and maintained by the Graphics and Vision Research group, University of Basel. 
The current maintainers of the project (people who can merge pull requests) are: 

* [Ghazi Bouabene](https://github.com/ghazi-bouabene)
* [Thomas Gerig](https://github.com/gerith)
* [Marcel Luethi](https://github.com/marcelluethi)
* [Andreas Forster](https://github.com/Andreas-Forster)
* [Dennis Madsen](https://github.com/madsendennis)
* [Dana Rahbani](https://github.com/DanaRahbani)
* [Patrick Kahr](https://github.com/kahrpatrick)

## Related Projects
Scalismo is closely related to the 
[statismo](http://www.github.com/statismo/statismo) project, 
and some of the scalismo developers are also actively working on statismo. 
In fact, scalismo had been started as an attempt to provide the core functionality of Statismo and the ITK registration toolkit, but without
the complexity that is induced by these toolkits. 

The design of the registration approach used in scalismo is strongly influenced by [ITK](http://www.itk.org) and [Elastix](http://elastix.isi.uu.nl).


## Copyright and License
All code is available to you under the Apache license, version 2, available at http://www.apache.org/licenses/LICENSE-2.0. 

Copyright, University of Basel, 2015.

