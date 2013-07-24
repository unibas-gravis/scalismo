organization := "ch.unibas.cs.gravis"

name := "SMPTk"

version := "0.1.0-SNAPSHOT"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)


libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
	      "org.scalatest" %% "scalatest" % "1.9" % "test",
	        "org.scalanlp" %% "breeze-math" % "0.2",
            "org.scalanlp" %% "breeze-learn" % "0.2",
            "org.scalanlp" %% "breeze-process" % "0.2",
            "org.scalanlp" %% "breeze-viz" % "0.2",
"org.scala-lang" % "scala-reflect" %"2.10.0",
	    "net.imagej" % "ij" % "latest.integration",
	    "org.scala-lang" % "scala-swing" % "2.10.0",
	    "com.twitter"   % "util-collection"   % "5.3.10"
)

resolvers ++= Seq(
   "Sonatype SNAPSHOTs" at "https://oss.sonatype.org/content/repositories/snapshots/",
   "imagej.releases" at "http://maven.imagej.net/content/repositories/releases",
   "twitter" at "http://maven.twttr.com/"
   )

fork := true

EclipseKeys.withSource := true

scalacOptions += "-feature"

scalaVersion := "2.10.0"
