import sbt.Resolver

val scalismoPlatform = {
  val env = System.getenv("SCALISMO_PLATFORM")
  if (env != null) env else "all"
}

Test / parallelExecution := false

lazy val root = (project in file("."))
  .settings(
    name := "scalismo",
    organization := "ch.unibas.cs.gravis",
    scalaVersion := "3.2.2",
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
    crossScalaVersions := Seq("2.13.6", "3.1.0"),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    scalacOptions ++= {
      Seq(
        "-encoding",
        "UTF-8",
        "-feature",
        "-language:implicitConversions"
        // disabled during the migration
        // "-Xfatal-warnings"
      ) ++
        (CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, _)) =>
            Seq(
              "-unchecked",
              "-source:3.0-migration"
            )
          case _ =>
            Seq(
              "-deprecation",
              "-Wunused:imports,privates,locals",
              "-Wvalue-discard"
            )
        })
    },
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.15",
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      ("org.scalanlp" %% "breeze" % "2.1.0"),
      ("org.scalanlp" %% "breeze-natives" % "2.1.0"),
      "ch.unibas.cs.gravis" % "scalismo-niftijiojar" % "0.1.0",
      "com.lihaoyi" %% "upickle" % "2.0.0",
      "ch.unibas.cs.gravis" % "hdf5javanatives" % "0.1.0",
      "ch.unibas.cs.gravis" % "vtkjavanativesall" % "0.1.1",
      "io.jhdf" % "jhdf" % "0.6.9",
      "org.slf4j" % "slf4j-nop" % "1.6.0" // this silences slf4j complaints in registration classes
    ),
    libraryDependencies ++= (scalaBinaryVersion.value match {
      case "3" =>
        Seq(
          "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"
        )
      case "2.13" =>
        Seq(
          "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
        )
      case _ => { println(scalaBinaryVersion.value); Seq() }
    })
  )
  .enablePlugins(GitVersioning)
  .settings(
    git.baseVersion := "develop",
    git.useGitDescribe := false,
    useJGit
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
