import sbt.Resolver

ThisBuild / version := "0.92.1"

Test / parallelExecution := false

lazy val root = (project in file("."))
  .settings(
    name := "scalismo",
    organization := "ch.unibas.cs.gravis",
    scalaVersion := "3.3.0",
    homepage := Some(url("https://scalismo.org")),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(
      ScmInfo(url("https://github.com/unibas-gravis/scalismo"), "git@github.com:unibas-gravis/scalismo.git")
    ),
    developers := List(
      Developer("marcelluethi", "marcelluethi", "marcel.luethi@unibas.ch", url("https://github.com/marcelluethi"))
    ),
    publishMavenStyle := true,
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases")
    ),
    scalacOptions ++= {
      Seq(
        "-encoding",
        "UTF-8",
        "-feature",
        "-language:implicitConversions",
        "-unchecked"
        // disabled during the migration
        // "-Xfatal-warnings"
      )
    },
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.16",
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.scalanlp" %% "breeze-natives" % "2.1.0",
      "ch.unibas.cs.gravis" % "scalismo-niftijiojar" % "0.1.0",
      "ch.unibas.cs.gravis" %% "scalismo-hdf5-json" % "0.1-RC1",
      "ch.unibas.cs.gravis" % "vtkjavanativesall" % "0.2-RC1",
      "io.jhdf" % "jhdf" % "0.6.10",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.slf4j" % "slf4j-nop" % "2.0.7", // this silences slf4j complaints in registration classes
      "com.jme3" % "jmonkeyengine3" % "3.0.0.20140325-SNAPSHOT",
      "com.jme3" % "jME3-core" % "3.0.0.20140325-SNAPSHOT"
    )
  )
  .enablePlugins(GitBranchPrompt)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
    buildInfoPackage := "scalismo"
  )
  .enablePlugins(GhpagesPlugin)
  .settings(
    git.remoteRepo := "git@github.com:unibas-gravis/scalismo.git"
  )
  .enablePlugins(SiteScaladocPlugin)
