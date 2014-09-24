import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
        
object BuildSettings {
  val buildOrganization = "org.statismo"
  val buildVersion = "0.3.6"
  val buildScalaVersion = "2.10.4"
  val publishURL = Resolver.file("file", new File("/export/contrib/statismo/repo/public"))

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
  private val sonatypeSnapshots = "Sonatype SNAPSHOTs" at "https://oss.sonatype.org/content/repositories/snapshots/"
  private val sonatypeRelease = "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  private val imagej = "imagej.releases" at "http://maven.imagej.net/content/repositories/releases"
  private val twitter = "twitter" at "http://maven.twttr.com/"
  private val statismoPublic = "statismo" at "http://statismo.cs.unibas.ch/repository/public"
  val stkResolvers = Seq(statismoPublic, sonatypeSnapshots, sonatypeRelease, imagej, twitter)
}

object Dependencies {
  import BuildSettings.buildScalaVersion
  val commonsio = "org.apache.commons" % "commons-io" % "1.3.2"
  val scalatest = "org.scalatest" %% "scalatest" % "1.9" % "test"
  val breezeMath = "org.scalanlp" % "breeze_2.10" % "0.7"
  val breezeNative = "org.scalanlp" % "breeze-natives_2.10" % "0.7"
  val statismoNativelibs = "org.statismo" %% "nativelibs" % "1.3.1"
}

object STKBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq(
    scalatest,
    breezeMath,
    breezeNative,
    statismoNativelibs,
    commonsio)

  lazy val cdap2 = Project(
    "stkcore",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= commonDeps,
      resolvers ++= stkResolvers,
      publishTo := Some(publishURL),
      EclipseKeys.withSource := true))
}
