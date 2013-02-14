organization := "ch.unibas.cs.gravis"

name := "SMPTk"

version := "0.1.0-SNAPSHOT"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)


libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
	      "org.scalatest" %% "scalatest" % "1.9" % "test",
            "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
            "org.scalanlp" %% "breeze-learn" % "0.2-SNAPSHOT",
            "org.scalanlp" %% "breeze-process" % "0.2-SNAPSHOT",
            "org.scalanlp" %% "breeze-viz" % "0.2-SNAPSHOT",
	    "org.scala-lang" % "scala-reflect" %"2.10.0"
)

resolvers ++= Seq(
   "Sonatype SNAPSHOTs" at "https://oss.sonatype.org/content/repositories/snapshots/")





EclipseKeys.withSource := true

scalacOptions += "-feature"

scalaVersion := "2.10.0"
