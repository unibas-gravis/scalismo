import com.typesafe.sbt.{GitBranchPrompt, GitVersioning}
import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.SbtGhPages._
import com.typesafe.sbt.SbtGit.git

import sbtbuildinfo.Plugin._
import com.typesafe.sbt.SbtGit.useJGit

object BuildSettings {
  val buildOrganization = "ch.unibas.cs.gravis"
  val buildScalaVersion = "2.12.6"


  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := Seq("2.11.12", "2.12.6"),
    javacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2,  11)) => Seq("-source", "1.6", "-target", "1.6")
      case _ => Seq()
    }),
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2,  11)) =>  Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature", "-target:jvm-1.6")
      case _ => Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature", "-target:jvm-1.8")
    })
  )

  // nativelibs implementation to use (e.g., "linux64"). If not explicitly set, use "all"
  // which contains all supported platforms.
  val scalismoPlatform = {
    val env = System.getenv("SCALISMO_PLATFORM")
    if (env != null) env else "all"
  }
}

object Resolvers {
   val stkResolvers = Seq(Resolver.jcenterRepo, Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots"))
}

object Dependencies {
  import BuildSettings.scalismoPlatform
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  val breezeMath = "org.scalanlp" %% "breeze" % "0.13"
  val breezeNative = "org.scalanlp" %% "breeze-natives" % "0.13"
  val sprayJson = "io.spray" %% "spray-json" % "1.3.3"
  val scalismoNativeStub = "ch.unibas.cs.gravis" % "scalismo-native-stub" % "4.0.1"
  val scalismoNativeImpl = "ch.unibas.cs.gravis" % s"scalismo-native-$scalismoPlatform" % "4.0.1" % "test"

  val slf4jNop = "org.slf4j" % "slf4j-nop" % "1.6.0" // this silences slf4j complaints in registration classes
}

object STKBuild extends Build {

  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val scalismo = Project(
    "scalismo",
    file("."),
    settings = buildSettings ++ Seq(
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
      libraryDependencies ++= commonDeps,
      resolvers ++= stkResolvers,
      parallelExecution in Test := false,
      EclipseKeys.withSource := true)
      ++ site.settings
      ++ site.includeScaladoc()
      ++ ghpages.settings ++
      Seq(
        git.remoteRepo := "git@github.com:unibas-gravis/scalismo.git"
      )++
      Seq(
        git.baseVersion := "develop",
        git.useGitDescribe := false,
        useJGit
      ) ++
      buildInfoSettings ++
      Seq(
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
      buildInfoPackage := "scalismo"
      )
).enablePlugins(GitBranchPrompt, GitVersioning)


  // Sub-project specific dependencies
  val commonDeps = Seq(
    scalatest,
    breezeMath,
    breezeNative,
    scalismoNativeStub,
    scalismoNativeImpl,
    sprayJson,
    slf4jNop
  )
}
