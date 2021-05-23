resolvers += Resolver.url("bintray-sbt-plugin-releases", url("https://dl.bintray.com/banno/oss"))(
  Resolver.ivyStylePatterns
)

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.4.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.2.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")
