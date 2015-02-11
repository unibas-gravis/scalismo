# Scalismo - Scalable Shape Modelling

Scalismo is a library for shape modeling and model-based image analysis in Scala. 
It has its origin in the research done at the [Graphics and Vision Research Group](gravis.cs.unibas.ch) at the [University of Basel](www.unibas.ch). 

The vision of the project is to build a system that is:
* as productive for research prototypes as Matlab or Python, 
* makes it possible to build safe and reliable software even for complex pipelines and full applications. 


## Documentation
* [Quickstart](shapemodelling.cs.unibas.ch/quickstart)
* [Graphical Tutorial](shapemodelling.cs.unibas.ch/tutorial)
* [API Doc](shapemodelling.cs.unibas.ch/scalismo-api)

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
to support the needs that arise in the research at the Gravis and Vision research group. 
Thus, a lot of methods that you would find in more mature toolkits such as [ITK](www.itk.org) are still missing. 

If you find scalismo useful for your work, but find that some functionality is missing, 
please help us to make it more complete by submitting a pull request. 

We are also always grateful if you report bugs or participate in discussions regarding future developments on the mailing list. 

## Maintainers
The project is currently developed and maintained by the Graphics and Vision Research group, University of Basel. 
The current maintainers of the project (people who can merge pull requests) are: 
* Ghazi Bouabene ghazi.bouabene@unibas.ch
* Thomas Gerig thomas.gerig@unibas.ch
* Christoph Langguth christoph.langguth@unibas.ch
* Marcel Luethi marcel.luethi@unibas.ch

## Related Projects
Scalismo is developed by the same group and shares the same theoretical foundations as the 
[statismo](www.github.com/statismo/statismo) project, which is a C++ framework for shape modeling. 

The design of scalismo is strongly influenced by [ITK](www.itk.org), [VTK](www.vtk.org) and [Elastix](elastix.isi.uu.nl).

## Copyright and License
All code is available to you under the Apache license, version 2, available at http://www.apache.org/licenses/LICENSE-2.0. 

Copyright, University of Basel, 2015.

