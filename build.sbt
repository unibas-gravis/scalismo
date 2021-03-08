val scalismoPlatform = {
  val env = System.getenv("SCALISMO_PLATFORM")
  if (env != null) env else "all"
}

lazy val root = (project in file("."))
  .settings(
    name := "scalismo",
    organization := "ch.unibas.cs.gravis",
    scalaVersion := "2.13.1",
    homepage := Some(url("https://scalismo.org")),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(ScmInfo(url("https://github.com/unibas-gravis/scalismo"), "git@github.com:unibas-gravis/scalismo.git")),
    developers := List(Developer("marcelluethi", "marcelluethi", "marcel.luethi@unibas.ch", url("https://github.com/marcelluethi"))),
    publishMavenStyle := true,
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    crossScalaVersions := Seq("2.12.11", "2.13.1"),
    resolvers ++= Seq(
      Resolver.bintrayRepo("unibas-gravis", "maven"),
      Resolver.jcenterRepo,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
    ),
    scalacOptions += "-deprecation",
    javacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => Seq("-source", "1.6", "-target", "1.6")
      case _             => Seq("-source", "1.8", "-target", "1.8")
    }),
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) =>
        Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature", "-target:jvm-1.6")
      case _ => Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature", "-target:jvm-1.8")
    }),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
      "org.scalanlp" %% "breeze" % "1.0",
      "org.scalanlp" %% "breeze-natives" % "1.0",
      "io.spray" %% "spray-json" % "1.3.5",
      "ch.unibas.cs.gravis" % "scalismo-native-stub" % "4.0.1",
      "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.1" % "test",
      "org.slf4j" % "slf4j-nop" % "1.6.0" // this silences slf4j complaints in registration classes
    ),
    libraryDependencies ++= (scalaBinaryVersion.value match {
      case "2.13" => Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
      case _      => Seq()
    }),
    unmanagedSourceDirectories in Compile += {
      val sourceDir = (sourceDirectory in Compile).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
        case _                       => sourceDir / "scala-2.13-"
      }
    }
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
