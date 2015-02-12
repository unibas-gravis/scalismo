# Scalismo - Scalable Image Analysis and Shape Modelling

Scalismo is a library for statistical shape modeling and model-based image analysis in Scala. 
It has its origin in the research done at the [Graphics and Vision Research Group](gravis.cs.unibas.ch) at the [University of Basel](www.unibas.ch). 

The vision of the project is to provide an environment for modelling and image analysis which 

* makes it easy and fun to try out ideas and build research prototypes
* is powerful enough to build full-scale industrial applications
* makes it feasible to deploy it in complex, distributed imaging pipelines. 

We aim to achieve these properties by leveraging two core technologies:

* A simple but versatile [mathematical approach](http://gravis.cs.unibas.ch/publications/2013/MLMI-Luethi_etal-2013.pdf) to shape modeling and registration, based on the theory of Gaussian processes.
* The Scala and Java ecosystem for reducing the software complexity. 

## Documentation
* [Quickstart](shapemodelling.cs.unibas.ch/quickstart)
* [Graphical Tutorial](shapemodelling.cs.unibas.ch/tutorial)
* [API Doc](shapemodelling.cs.unibas.ch/scalismo-api/latest)

There is also a [scalismo google group](https://groups.google.com/forum/#!forum/scalismo) for general questions and discussions. 

## Using scalismo

To use scalismo in your own project, add the following lines to your build.sbt
```
resolvers ++= Seq(
	  "shapemodelling unibas" at "http://shapemodelling.cs.unibas.ch/repository/public"
	  )

libraryDependencies  ++= Seq(
            // other dependencies here
 	    "ch.unibas.cs.gravis" %% "scalismo" % "0.7.0"
	    // scalismo depends on some native libraries, which must be added explicitly for your system
	    "ch.unibas.cs.gravis" % "scalismo-native-all" % "2.0.1" 
)
```

Scalismo works best together with a GUI toolkit, scalismo-ui, which allows you to quickly visualize shapes and shape models. 
To use scalismo-ui (which is currently not open source) also add the following dependency

```
libraryDependencies  ++= Seq(
	    "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.7.0"
)
```

### Building scalismo 
To build scalismo, run ```sbt``` and then any of the following commands

* ```compile```: Compile sbt
* ```test```: Run the unit test
* ```doc```: Generate the api docs
* ```scalastyle```: Run the style checker


## Participation
While scalismo is already fully usable for shape modeling and simple image processing task, its functionality is currently targeted
to support the needs that arise in the research at the Gravis and Vision research group.  In particular, many standard image and mesh 
processing algorithms are missing.

If you find scalismo useful for your work, you can help us to make it more complete by implementing missing features, in particular 
filters for image and mesh processing, and support for more image formats. 

We are also always grateful if you report bugs or if give us feedback on how you use scalismo in your work and how you think we can improve it. 

## Maintainers
The project is currently developed and maintained by the Graphics and Vision Research group, University of Basel. 
The current maintainers of the project (people who can merge pull requests) are: 

* Ghazi Bouabene ghazi.bouabene@unibas.ch
* Thomas Gerig thomas.gerig@unibas.ch
* Christoph Langguth christoph.langguth@unibas.ch
* Marcel Luethi marcel.luethi@unibas.ch

## Related Projects
Scalismo is closely related to the 
[statismo](http://www.github.com/statismo/statismo) project, 
and some of the scalismo developers are also actively working on statismo. 
In fact, scalismo had been started as an attempt to provide the core functionality of Statismo and the ITK registration toolkit, but without
the complexity that is induced by these toolkits. 

The design of the registration approach used in scalismo is strongly influenced by [ITK](www.itk.org) and [Elastix](elastix.isi.uu.nl).


## Copyright and License
All code is available to you under the Apache license, version 2, available at http://www.apache.org/licenses/LICENSE-2.0. 

Copyright, University of Basel, 2015.

