import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.SbtGhPages._
import com.typesafe.sbt.SbtGit.git

import com.banno.license.Plugin.LicenseKeys._
import com.banno.license.Licenses._

object BuildSettings {
  val buildOrganization = "ch.unibas.cs.gravis"
  val buildVersion = "develop-SNAPSHOT"
  val buildScalaVersion = "2.10.4"
  val publishURL = Resolver.file("file", new File("/export/contrib/statismo/repo/public"))

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := Seq("2.10.4", "2.11.4"),
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    scalacOptions ++= Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature", "-target:jvm-1.6"),
    shellPrompt := ShellPrompt.buildShellPrompt)
}

// Shell prompt which show the current project,
// git branch and build version
object ShellPrompt {
  val buildShellPrompt = {
    (state: State) =>
      {
        val currProject = Project.extract(state).currentProject.id
        "%s:%s:%s> ".format(
          currProject, currBranch, BuildSettings.buildVersion)
      }
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
    getOrElse "-" stripPrefix "## ")

  object devnull extends ProcessLogger {
    def info(s: => String) {}
    def error(s: => String) {}
    def buffer[T](f: => T): T = f
  }
}

object Resolvers {
  private val sonatypeSnapshots = "Sonatype SNAPSHOTs" at "https://oss.sonatype.org/content/repositories/snapshots/"
  private val sonatypeRelease = "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  private val scalismoPublic = "scalismo" at "http://shapemodelling.cs.unibas.ch/repository/public"

  val stkResolvers = Seq(scalismoPublic, sonatypeSnapshots, sonatypeRelease)
}

object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "2.2+" % "test"
  val breezeMath = "org.scalanlp" %% "breeze" % "0.11+"
  val breezeNative = "org.scalanlp" %% "breeze-natives" % "0.11+"
  val sprayJson = "io.spray" %% "spray-json" % "1.2.6"
  val scalismoNativeStub = "ch.unibas.cs.gravis" % "scalismo-native-stub" % "2.0.+"
  val scalismoNativeImpl = "ch.unibas.cs.gravis" % "scalismo-native-all" % "2.0.+" % "test"
  val spire = "org.spire-math" %% "spire" % "0.9.0"
}

object STKBuild extends Build {

  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val lic = com.banno.license.Plugin.licenseSettings ++ Seq(license := apache2("Copyright 2015 University of Basel, Graphics and Vision Research Group"), removeExistingHeaderBlock := false)
  
  lazy val scalismo = Project(
    "scalismo",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= commonDeps,
      resolvers ++= stkResolvers,
      parallelExecution in Test := false,
      publishTo := Some(publishURL),
      EclipseKeys.withSource := true)
      ++ site.settings 
      ++ site.includeScaladoc()
      ++ ghpages.settings ++
      Seq(
        git.remoteRepo := "git@github.com:unibas-gravis/scalismo.git"
      )
)
  // Sub-project specific dependencies
  val commonDeps = Seq(
    scalatest,
    breezeMath,
    breezeNative,
    scalismoNativeStub,
    scalismoNativeImpl,
    sprayJson,
    spire)
}
