import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
        
object BuildSettings {
  val buildOrganization = "org.statismo"
  val buildVersion = "0.1.0-SNAPSHOT"
  val buildScalaVersion = "2.10.2"
  val publishURL = Resolver.file("file", new File("/export/contrib/statismo/repo"))

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt := ShellPrompt.buildShellPrompt)
}

// Shell prompt which show the current project,
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info(s: => String) {}
    def error(s: => String) {}
    def buffer[T](f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
    getOrElse "-" stripPrefix "## ")

  val buildShellPrompt = {
    (state: State) =>
      {
        val currProject = Project.extract(state).currentProject.id
        "%s:%s:%s> ".format(
          currProject, currBranch, BuildSettings.buildVersion)
      }
  }
}

object Resolvers {
  val sonatypeSnapshots = "Sonatype SNAPSHOTs" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val sonatypeRelease = "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  val imagej = "imagej.releases" at "http://maven.imagej.net/content/repositories/releases"
  val twitter = "twitter" at "http://maven.twttr.com/"
  val statismoSnapshot = "statismo" at "file:///export/contrib/statismo/repo"

  val stkResolvers = Seq(sonatypeSnapshots, sonatypeRelease, imagej, twitter, statismoSnapshot)
}

object Dependencies {

  val scalatest = "org.scalatest" %% "scalatest" % "1.9" % "test"
  val breezeMath = "org.scalanlp" %% "breeze-math" % "0.4"
  val breezeViz = "org.scalanlp" %% "breeze-viz" % "0.4"
  val scalaReflect = "org.scala-lang" % "scala-reflect" % "2.10.0"
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.10.0"
  val twitterUtilCollection = "com.twitter" % "util-collection" % "5.3.10"
  val twitterUtilEval = "com.twitter" %% "util-eval" % "6.2.4"
  val statismoNativelibs = "org.statismo" %% "nativelibs" % "0.1.0-SNAPSHOT"
}

object STKBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq(
    scalatest,
    breezeMath,
    breezeViz,
    scalaReflect,
    scalaSwing,
    statismoNativelibs)

  lazy val cdap2 = Project(
    "stkcore",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= commonDeps,
      resolvers ++= stkResolvers,
      publishTo := Some(publishURL),
      EclipseKeys.withSource := true))
}
